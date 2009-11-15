use strict;
use warnings;
use Test::More tests => 6;
use Test::Exception;

use Stylish::Server;

my $server = Stylish::Server->new;
ok $server, 'got server';

my $obj;
lives_ok {
    $obj = $server->load_plugin('LoadPlugin');
} 'LoadPlugin loads';

ok $server->has_plugin('load_plugin'), 'renamed and loaded ok';

my $r;
lives_ok {
    $r = $server->handle_message('load_plugin', {
        plugin => 'LoadPlugin',
        token  => 42,
    });
} 'handles load_plugin message ok';

is $r->{token}, 42, 'got token';
is $r->{id}, 0+$obj, 'got same id';
