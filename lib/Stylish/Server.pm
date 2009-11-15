use MooseX::Declare;

class Stylish::Server with MooseX::Runnable {
    use Set::Object;
    use MooseX::Types::Set::Object;
    use AnyEvent;
    use AnyEvent::Socket qw(tcp_server);

    our $VERSION = '0.00_00';
    our $AUTHORITY = 'github:jrockway';

    use Stylish::Server::Connection;

    has '_connections' => (
        init_arg => undef,
        isa      => 'Set::Object',
        required => 1,
        default  => sub { Set::Object->new },
        handles  => {
            'add_connection'    => 'insert',
            'delete_connection' => 'remove',
            'connections'       => 'members',
        },
    );

    has '_plugins' => (
        init_arg => undef,
        traits   => ['Hash'],
        isa      => 'HashRef',
        required => 1,
        default  => sub { +{} },
        handles  => {
            'install_plugin' => 'set',
            'loaded_plugins' => 'keys',
            'get_plugin'     => 'get',
            'has_plugin'     => 'exists',
        },
    );

    method handle_message(Str $type, HashRef $payload){
        my $handler = $self->get_plugin($type);
        die "no handler for '$type'" unless $handler;

        my $token = delete $payload->{token};

        my $result_hashref = $handler->(%$payload);
        die "'$type' handler returned non-hashref ('$result_hashref')"
          unless ref $result_hashref eq 'HASH';

        $result_hashref->{token} = $token if defined $token;
        return $result_hashref;
    }

    method name_plugin(Str $class){
        $class =~ s/^[+]//;
        $class =~ s/::/_/g;
        $class =~ s/([a-z])([A-Z])/${1}_$2/g;
        return lc $class;
    }

    method load_plugin(Str $name, HashRef $args?){
        my $short_name = $self->name_plugin($name);
        return $self->get_plugin($short_name) if $self->has_plugin($short_name);

        my $class = $name;
        $class = "Stylish::Plugin::$class" unless $class =~ s/^[+]//; # effectful.
        Class::MOP::load_class($class);

        $args ||= {};
        $args->{server} = $self;
        my $instance = $class->new( $args );
        confess "$class does not do Stylish::Plugin, so can't use as a Stylish plugin"
          unless eval { $class->does('Stylish::Plugin') };

        my $code = sub { $instance->do_request( @_ ) };
        $self->install_plugin( $short_name => $code );
        return $code;
    }

    method start {
        tcp_server undef, 36228, sub {
            my ($fh, $host, $port) = @_;
            my $c; $c = Stylish::Server::Connection->new(
                client_info => { host => $host, port => $port },
                server      => $self,
                handle      => AnyEvent::Handle->new(
                    fh       => $fh,
                    on_error => sub { $self->delete_connection($c) },
                    on_eof   => sub { $self->delete_connection($c) },
                ),
            );
            $self->add_connection($c);

            $c->write('welcome', { sessionid => 0+$c,
                                   version => "Stylish $AUTHORITY:$VERSION" });
        };
    }

    method run {
        $self->load_plugin('LoadPlugin');
        $self->start;
        EV::loop();
        return 0;
    }

}
