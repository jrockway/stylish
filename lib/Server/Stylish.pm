package Server::Stylish;
use MooseX::POE;
use Moose::Util::TypeConstraints;
use Scalar::Util qw(refaddr);
use POE qw(Component::Server::TCP);

our $VERSION = '0.01';

with qw/MooseX::LogDispatch::Levels/;

has 'formatter' => (
    is        => 'ro',
    does      => 'Server::Stylish::Format',
    default   => sub {
        require Server::Stylish::Format::Sexp;
        Server::Stylish::Format::Sexp->new;
    },
);

subtype PortNumber => Int => sub { $_ > 0 && $_ < 65536 };

has 'port' => (
    is      => 'ro',
    isa     => 'PortNumber',
    default => '36227',
);

has '_server_id' => (
    isa        => 'Int',
    is         => 'ro',
    accessor   => 'server',
    lazy_build => 1,
);

sub _mk_handler_dispatcher {
    my $self = shift;
    my $name = shift;

    (my $fname = $name) =~ s/([a-z])([A-Z])/$1_$2/;
    $fname = lc $fname;
    
    return ($name => sub { return $self->$fname(@_) });
}

sub _build__server_id {
    my $self = shift;
    POE::Component::Server::TCP->new(
        Port => $self->port,
        map { $self->_mk_handler_dispatcher($_) }
          qw/ClientInput ClientConnected ClientDisconnected ClientError/,
    );
}

before client_connected => sub {
    shift->debug('Client connected');
};

before client_disconnected => sub {
    shift->debug('Client disconnected');
};

before client_input => sub {
    my $self = shift;
    $self->debug('Client input:', $_[ARG0]);
};

sub output {
    my $self = shift;
    my $msg  = shift;
    my $heap = $_[HEAP];
    $heap->{client}->put($self->formatter->format($msg));
}

sub client_connected {
    my $self = shift;
    my $heap = $_[HEAP];
    
    $self->output(['welcome', 
                   ':version', ['Stylish', $VERSION],
                   ':session-id', refaddr $_[SESSION]],
                  @_);
}

sub client_disconnected {
    my ($self, @POE) = @_;
}

sub client_input {
    my $self = shift;
    my ($input, $heap, $session) = @_[ARG0, HEAP, SESSION];

    eval {
        my $parsed = $self->formatter->parse($input);
    };

    $heap->{client}->put("it worked, $session");
}


sub client_error {
    my ($self, @POE) = @_;
    $self->error('Client error:');
}

sub START {
    my $self = shift;
    $self->info('Starting Stylish server on port', $self->port);

    $poe_kernel->post( $self->server => register => 'all' );    
}

# We registered for all events, this will produce some debug info.
sub DEFAULT {
    my ( $self, $event, $args ) = @_[ OBJECT, ARG0 .. $#_ ];
    my @output = ("$event: ");

    foreach my $arg (@$args) {
        if ( ref($arg) eq ' ARRAY ' ) {
            push( @output, "[" . join( " ,", @$arg ) . "]" );
        }
        else {
            push( @output, "'$arg' " );
        }
    }
    $self->debug( join ' ', @output );
    return 0;
}

sub run { POE::Kernel->run }

1;
