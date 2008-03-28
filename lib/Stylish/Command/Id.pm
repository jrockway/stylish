package Stylish::Command::Id;
use Moose::Role;

sub command_id {
    my ($self, $client, @args) = @_;
    return ['id' => $client];
};

1;
