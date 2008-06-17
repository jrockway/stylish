use strict;
use warnings;
use Test::More tests => 4;

{ package Class;
  use Moose;
  has foo => ( is => 'ro', documentation => 'This is foo' );
  has bar => ( is => 'ro', );

  # just something to apply the command to
  package DS;
  use Moose;
  with 'Stylish::Command::Docstring';

}

my $ds = DS->new;
is $ds->_get_docstring(qw/Class attribute foo/),
  'This is foo', 'got correct text';

is $ds->_get_docstring(qw/Class attribute bar/),
  'No documentation found for Class::bar';  

is $ds->_get_docstring(qw/Class attribute baz/),
  'No attribute baz in Class';

is $ds->_get_docstring(qw/Class::Foo attribute quux/),
  'No class Class::Foo!';

  

