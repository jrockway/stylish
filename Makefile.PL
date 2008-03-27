use inc::Module::Install;

name 'Server-Stylish';
all_from 'lib/Server/Stylish.pm';

requires 'Moose';
requires 'Moose::Util::TypeConstraints';
requires 'PPI';
build_requires 'Test::More';
build_requires 'ok';

WriteAll();
