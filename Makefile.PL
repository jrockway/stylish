use inc::Module::Install;

name 'Stylish';
all_from 'lib/Stylish.pm';

requires 'Data::SExpression';
requires 'Devel::REPL';
requires 'File::Slurp';
requires 'IO::CaptureOutput';
requires 'Moose';
requires 'Moose::Util::TypeConstraints';
requires 'MooseX::LogDispatch::Levels';
requires 'POE';
requires 'POE::Component::Server::TCP';
requires 'PPI';
build_requires 'Test::More';
build_requires 'ok';

tests 't/format/*.t t/syntax/*.t t/*.t';

WriteAll();
