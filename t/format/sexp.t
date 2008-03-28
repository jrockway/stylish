use strict;
use warnings;
use Test::More tests => 3;

use Stylish::Format::Sexp;

my $format = Stylish::Format::Sexp->new;

my $command = $format->parse('(syntax-highlight 1234 "this is the source code")');
is_deeply $command, [qw/syntax_highlight 1234/, 'this is the source code'];

my $data = [
    [ [1,2], 'foo'],
    [ [2,3], 'bar'],
];

is $format->format($data), q{(((1 2) foo) ((2 3) bar))};
is $format->format(\"hello world
"), q{"hello world\\n"};
