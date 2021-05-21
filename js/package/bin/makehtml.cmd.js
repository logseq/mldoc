var yargs = require('yargs'),
    fs = require('fs'),
    Messenger = require('./messenger.js'),
    MO = require('../index').Mldoc;

    yargs.reset()
    .usage('Usage: mldoc makehtml [options]')
    .example('mldoc makehtml -i', 'Reads from stdin and outputs to stdout')
    .example('mldoc makehtml -i foo.org -o bar.html', 'Reads \'foo.org\' and writes to \'bar.html\'')
    .version()
    .alias('v', 'version')
    .config('c')
    .alias('c', 'config')
    .help('h')
    .alias('h', 'help')
    .option('i', {
        alias : 'input',
        describe: 'Input source. Usually a org file. If omitted or empty, reads from stdin',
        type: 'string'
    })
    .option('o', {
        alias : 'output',
        describe: 'Output target. Usually a html file. If omitted or empty, writes to stdout',
        type: 'string',
        default: false
    })
    .option('u', {
        alias : 'encoding',
        describe: 'Input encoding',
        type: 'string'
    })
    .option('a', {
        alias : 'append',
        describe: 'Append data to output instead of overwriting',
        type: 'string',
        default: false
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
    });

function run () {
    'use strict';
    var argv = yargs.argv,
        readMode = (!argv.i || argv.i === '') ? 'stdin' : 'file',
        writeMode = (!argv.o || argv.o === '') ? 'stdout' : 'file',
        msgMode = (writeMode === 'file') ? 'stdout' : 'stderr',
        /**
         * MSG object
         * @type {Messenger}
         */
        messenger = new Messenger(msgMode, argv.q, argv.m),
        read = (readMode === 'stdin') ? readFromStdIn : readFromFile,
        write = (writeMode === 'stdout') ? writeToStdOut : writeToFile,
        enc = argv.encoding || 'utf8',
        append = argv.a || false,
        content, html;
    var extension = argv.i.split('.').pop();
    var format = (extension === 'org') ? 'Org' : 'Markdown';
    messenger.printMsg('...');
    // read the input
    messenger.printMsg('Reading data from ' + readMode + '...');
    content = read(enc);

    // process the input
    messenger.printMsg('Parsing file...');
    // TODO: add config options
    html = MO.parseHtml(content, JSON.stringify({"toc": true,
                                                 "heading_number": true,
                                                 "keep_line_break": false,
                                                 "format": format,
                                                 "heading_to_list": false
                                                }));

    // write the output
    messenger.printMsg('Writing data to ' + writeMode + '...');
    write(html, append);
    messenger.okExit();

    function readFromStdIn () {
        try {
            var size = fs.fstatSync(process.stdin.fd).size;
            return size > 0 ? fs.readSync(process.stdin.fd, size)[0] : '';
        } catch (e) {
            var err = new Error('Could not read from stdin, reason: ' + e.message);
            messenger.errorExit(err);
        }
    }

    function readFromFile (encoding) {
        try {
            return fs.readFileSync(argv.i, encoding);
        } catch (err) {
            messenger.errorExit(err);
        }
    }

    function writeToStdOut (html) {
        return process.stdout.write(html);
    }

    function writeToFile (html, append) {
        // If a flag is passed, it means we should append instead of overwriting.
        // Only works with files, obviously
        var write = (append) ? fs.appendFileSync : fs.writeFileSync;
        try {
            write(argv.o, html);
        } catch (err) {
            messenger.errorExit(err);
        }
    }
}

module.exports = exports = {
    run: run
};
