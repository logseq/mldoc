var yargs = require('yargs'),
    fs = require('fs'),
    Messenger = require('./messenger.js'),
    MO = require('../index').Mldoc;

yargs.usage('Usage: mldoc convert [options]')
  .example('mldoc convert -i', 'Reads from stdin and outputs to stdout')
  .example('mldoc convert -i foo.org -o bar.html', 'Reads \'foo.org\' and writes to \'bar.html\'')
  .example('mldoc convert -i foo.md -f ast', 'Reads \'foo.md\' and writes its AST to stdout')
  .version()
  .alias('v', 'version')
  .config('c')
  .alias('c', 'config')
  .help('h')
  .alias('h', 'help')
  .option('i', {
    alias: 'input',
    describe: 'Input source. Usually a org file. If omitted or empty, reads from stdin',
    type: 'string'
  })
  .option('o', {
    alias: 'output',
    describe: 'Output target. Usually a html file. If omitted or empty, writes to stdout',
    type: 'string',
    default: false
  })
  .option('f', {
    alias: 'format',
    describe: 'Output format',
    type: 'string',
    default: false
  })
  .option('u', {
    alias: 'encoding',
    describe: 'Input encoding',
    type: 'string',
    default: 'utf8'
  })
  .option('a', {
    alias: 'append',
    describe: 'Append data to output instead of overwriting',
    type: 'boolean',
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
      messenger = new Messenger(msgMode, argv.quiet, argv.mute),
      read = (readMode === 'stdin') ? readFromStdIn : readFromFile,
      write = (writeMode === 'stdout') ? writeToStdOut : writeToFile,
      content, output_content, config;

  var extension = (readMode === 'file') ? argv.i.split('.').pop() : '';
  var format = (extension === 'org') ? 'Org' : 'Markdown';

  var to_extension = (writeMode === 'file') ? argv.o.split('.').pop() : '';
  var to_format = argv.format || to_extension || 'markdown';
  to_format = to_format.replace('md', 'markdown');

  messenger.printMsg('...');
  // read the input
  messenger.printMsg('Reading data from ' + readMode + '...');
  content = read(argv.encoding);

  // process the input
  messenger.printMsg('Parsing file...');
  messenger.printMsg('Converting to ' + to_format);

  // TODO: add config options
  config = JSON.stringify({
    "toc": false,
    "parse_outline_only": false,
    "heading_number": false,
    "keep_line_break": false,
    "format": format,
    "heading_to_list": false,
    "exporting_keep_properties": true,
    "inline_type_with_pos": false,
    "export_md_remove_options": [],
    "hiccup_in_block": true,
  });
  if (to_format === 'ast')
    output_content = JSON.stringify(
      JSON.parse( MO.parseInlineJson(content, config) ),
      null, 4);
  else
    output_content = MO.export(to_format, content, config, '{}');

  // write the output
  messenger.printMsg('Writing data to ' + writeMode + '...');
  write(output_content, argv.append);
  messenger.okExit();

  function readFromStdIn () {
    try {
      var size = fs.fstatSync(process.stdin.fd).size;
      if (size === 0)
        return ''
      const buffer = Buffer.alloc(size)
      fs.readSync(process.stdin.fd, buffer)
      return buffer.toString(argv.encoding)
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

  function writeToStdOut (file) {
    return process.stdout.write(file);
  }

  function writeToFile (file, append) {
    // If a flag is passed, it means we should append instead of overwriting.
    // Only works with files, obviously
    var write = (append) ? fs.appendFileSync : fs.writeFileSync;
    try {
      write(argv.o, file);
    } catch (err) {
      messenger.errorExit(err);
    }
  }
}

module.exports = exports = {
  run: run
};
