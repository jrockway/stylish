use inc::Module::Install;

name 'Server-Stylish';
all_from 'lib/Server/Stylish.pm';

requires 'Data::SExpression';
requires 'Moose';
requires 'Moose::Util::TypeConstraints';
requires 'PPI';
requires 'POE';
requires 'POE::Component::Server::TCP';
requires 'MooseX::LogDispatch::Levels';
build_requires 'Test::More';
build_requires 'ok';

tests 't/format/*.t t/syntax/*.t t/*.t';

WriteAll();
