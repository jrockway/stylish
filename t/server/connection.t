use strict;
use warnings;
use Test::More tests => 3;

my $written;

BEGIN {
    package AnyEvent::Handle;
    sub new { bless {} }
    sub push_write { $written = [@_] }
    sub push_read {}
    sub push_shutdown {}
    $INC{'AnyEvent/Handle.pm'} = 1;
}
use Stylish::Server;

my $server = Stylish::Server->new;
ok $server;

$server->add_connection(
    Stylish::Server::Connection->new(
        client_info => { name => 'emacs' },
        server      => $server,
        handle      => AnyEvent::Handle->new(),
    ),
);

my ($connection) = $server->connections;
ok $connection, 'got connection';

my $foo = 0;
$server->install_plugin(
    'foo' => sub { $foo = $_[0]->{foo} },
);

$connection->handle_message( foo => { foo => 42 } );
is $foo, 42, 'handled foo message';
