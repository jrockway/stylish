package Stylish::Command::REPL;
use Moose::Role;

use Stylish::REPL;

# TODO: per-client?
has 'REPL' => (
    is      => 'ro',
    isa     => 'Stylish::REPL',
    default => sub {
        Stylish::REPL->new;
    },
);

sub command_repl {
    my ($self, $client, $string) = @_;

    # handlers for output/error
    my $status;
    my $result;
    my $get_output = sub { 
        $result = join '', @_;
        $status = 'success';
    };
    my $get_error = sub {
        $result = join ': ', @_; # XXX
        $status = 'error';
    };
      
    # tell them about us
    $self->REPL->output($get_output);
    $self->REPL->error($get_error);

    # eval the request (and format if there are no errors)
    my @result = $self->REPL->eval($string);
    $self->REPL->print(@result) unless $status;
    
    return ['repl', ":$status", \$result];
}

1;
