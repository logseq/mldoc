#!/usr/bin/env node

'use strict';

var yargs = require('yargs');

yargs
    .version()
    .alias('v', 'version')
    .option('h', {
        alias: 'help',
        description: 'Show help'
    })
    .option('q', {
        alias: 'quiet',
        description: 'Quiet mode. Only print errors',
        type: 'boolean',
        default: false
    })
    .option('m', {
        alias: 'mute',
        description: 'Mute mode. Does not print anything',
        type: 'boolean',
        default: false
    })
    .usage('Usage: mldoc_org <command> [options]')
    .demand(1, 'You must provide a valid command')
    .command('makehtml', 'Converts org mode into html')
    .example('mldoc_org makehtml -i foo.org -o bar.html', 'Converts \'foo.org\' to \'bar.html\'')
    .wrap(yargs.terminalWidth());

var argv = yargs.argv,
    command = argv._[0];

if (command === 'makehtml') {
    require('./makehtml.cmd.js').run();
} else {
    yargs.showHelp();
}

if (argv.help) {
    yargs.showHelp();
}
process.exit(0);
