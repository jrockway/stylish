package Stylish::Server;
use MooseX::POE;
use Moose::Util::TypeConstraints;
use Scalar::Util qw(refaddr);
use IO::Socket::INET;
use POE qw(Wheel::ListenAccept Wheel::ReadWrite);
use MooseX::AttributeHelpers;

our $VERSION = '0.01';

with qw/MooseX::LogDispatch::Levels
        Stylish::Command::Id
        Stylish::Command::Syntax
       /;

has 'formatter' => (
    is        => 'ro',
    does      => 'Stylish::Format',
    default   => sub {
        require Stylish::Format::Sexp;
        Stylish::Format::Sexp->new;
    },
);

subtype PortNumber => Int => sub { $_ > 0 && $_ < 65536 };

has 'port' => (
    is      => 'ro',
    isa     => 'PortNumber',
    default => '36227',
);

has 'server_socket' => (
    is      => 'ro',
    isa     => 'IO::Socket',
    lazy    => 1,
    default => sub {
        my $self = shift;
        return IO::Socket::INET->new( 
            LocalPort => $self->port,
            Listen => 10,
            Reuse  => "yes",
        ) or die "can't make server socket: $@\n";
    },
);

has 'server' => (
    is  => 'rw',
    isa => 'POE::Wheel::ListenAccept',
);

has 'clients' => (
    metaclass => 'Collection::Hash',
    is        => 'rw',
    isa       => 'HashRef[POE::Wheel::ReadWrite]',
    default   => sub { {} },
    provides  => {
        delete => 'delete_client',
        get    => 'get_client',
        set    => 'add_client',
    },
);

around add_client => sub {
    my ($next, $self, $wheel) = @_;
    $self->$next($wheel->ID, $wheel);
    return $wheel->ID;
};

sub START {
    my $self = $_[OBJECT];
    $self->info('Starting Stylish server on port', $self->port);

    $self->server(
        POE::Wheel::ListenAccept->new(
            Handle => $self->server_socket,
            AcceptEvent => "server_accepted",
            #ErrorEvent  => "server_error",
        ),
    );
}

event server_accepted => sub {
    my $self = $_[OBJECT];
    my $client_socket = $_[ARG0];

    my $wheel = POE::Wheel::ReadWrite->new(
        Handle     => $client_socket,
        InputEvent => "client_input",
        ErrorEvent => "client_error",
    );

    my $id = $self->add_client($wheel);
    $self->debug('Created new client with ID', $id);

    $self->output( $id, 
                   ['welcome', 
                    [ # information alist
                        [':version', 'Stylish', $VERSION],
                        [':session-id', $id],
                    ]]);
};

sub output {
    my ($self, $client, $msg) = @_;
    my $res = $self->formatter->format($msg);
    
    $self->debug('Response:', $res);
    $self->get_client($client)->put($res);

};

event 'client_input' => sub {
    my ($self, $input, $client)  = @_[OBJECT, ARG0, ARG1];
    
    my $result = eval {
        my @parsed = map { eval { $_->stringify } || $_ } 
          @{scalar $self->formatter->parse($input)};
        
        my $command  = shift @parsed;
        $self->debug("Calling '$command' with args", join ':', @parsed);
        my $method = "command_$command";
        my $r = $self->$method($client, @parsed);
        $self->debug("Got result from '$command': ", $r);
        return $r;
    };
    
    if(!$result){
        $self->output($client, [ error => parse_error => \"$@" ]);
    }
    else {
        $result = [$result] if ref $result ne 'ARRAY';
        $self->output($client, $result);
    }
};

event 'client_error' => sub {
    my $self = $_[OBJECT];
    $self->error('Client error, deleting client ID', $_[ARG3]);
    $self->delete_client($_[ARG3]);
};

sub run { POE::Kernel->run }

1;
