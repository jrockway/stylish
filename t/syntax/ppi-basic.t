use strict;
use warnings;
use Test::More tests => 3;

use Stylish::Syntax::PPI;

my $doc = Stylish::Syntax::PPI->new(document => <<'PERL');
use strict; 
use Foo::Bar 0.1234;
use Quux::Baz;
PERL

ok $doc, 'got the document';

my $syntax = $doc->syntax;
ok $syntax;

is_deeply $syntax,
  [
      [[ 1,4 ], 'keyword'],
      [[ 5,11 ], 'module-name'],
      [[ 14,17 ], 'keyword'],
      [[ 18,26 ], 'module-name'],
      [[ 35,38 ], 'keyword'],
      [[ 39,48 ], 'module-name'],
  ], 'syntax highlights applied correctly';

