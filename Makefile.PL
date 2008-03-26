use inc::Module::Install;

name 'Server-Stylish';
all_from 'lib/Server/Stylish.pm';

build_requires 'Test::More';
build_requires 'ok';

WriteAll();
