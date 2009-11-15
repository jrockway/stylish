use MooseX::Declare;

class Stylish::Server::Connection {
    use AnyEvent::Handle;
    use Try::Tiny;

    has 'client_info' => (
        is       => 'ro',
        isa      => 'HashRef',
        required => 1
    );

    has 'handle' => (
        is       => 'ro',
        isa      => 'AnyEvent::Handle',
        required => 1,
        handles  => [qw/push_write push_read push_shutdown/],
    );

    has 'server' => (
        is       => 'ro',
        isa      => 'Stylish::Server',
        required => 1,
        weak_ref => 1,
        handles  => { '_handle_message' => 'handle_message' },
    );

    has 'loop' => (
        is        => 'rw',
        isa       => 'CodeRef',
        predicate => 'has_loop',
        clearer   => 'clear_loop',
    );

    has 'state' => (
        is       => 'ro',
        isa      => 'HashRef',
        required => 1,
        default  => sub { +{} },
    );

    method write(Str $producer, HashRef $results) {
        $self->push_write( json => {
            producer => $producer,
            results  => $results,
        });
        warn "$producer: ", join ':', %$results;
    }

    method write_error(Str $error){
        $error =~ s/at .+$//;
        $self->write('error', { message => $error });
    }

    method handle_message(Str $type, HashRef $payload){
        warn "$type: ", join ':', %$payload;
        try {
            $self->write(
                $type => do { $self->_handle_message($type, $payload) || {} },
            );
        }
        catch {
            $self->write_error($_)
        }
    }

    method start_loop {
        return if $self->has_loop;
        $self->loop( sub {
            my ($h, $msg) = @_;

            try {
                my $type = $msg->{type} || die 'Message must have type';
                my $payload = $msg->{payload} || die 'Message must have payload';

                $self->handle_message($type, $payload);
            }
            catch {
                $self->write_error($_);
            };
            $self->push_read(json => $self->loop);
        });
        $self->push_read(json => $self->loop);
    }

    method end_loop {
        $self->loop( sub {
            $self->clear_loop;
        });
        $self->push_shutdown;
    }

    method BUILD {
        $self->start_loop;
    }

    method DEMOLISH {
        $self->end_loop;
    }
}
