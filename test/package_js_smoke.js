'use strict';

const assert = require('node:assert/strict');
const fs = require('node:fs');
const vm = require('node:vm');
const artifact = process.argv[2];
const config = JSON.stringify({ format: 'Markdown', keep_line_break: false });

function check(mldoc) {
  assert.ok(mldoc, 'existing Mldoc export');
  for (const input of ['', '- Package smoke λ with [[page]]\r\n']) {
    const ast = JSON.parse(mldoc.parseJson(input, config));
    assert.ok(Array.isArray(ast));
    if (input) assert.ok(ast.length > 0);
  }
  assert.ok(Array.isArray(JSON.parse(mldoc.parseInlineJson('[[page]] λ', config))));
  const opml = JSON.parse(mldoc.parseOPML(
    '<opml version="2.0"><head><title>smoke</title></head>' +
    '<body><outline text="Package smoke" _note="line1&#10;line2"/></body></opml>'
  ));
  assert.ok(JSON.stringify(opml).includes('line1\\nline2'), 'OPML attribute newline');
  assert.ok(mldoc.export('markdown', '- Package smoke λ\n', config, '{}').includes('Package smoke'));
}

check(require(artifact).Mldoc);
const browser = vm.createContext({ console, setTimeout, clearTimeout, TextEncoder, TextDecoder });
vm.runInContext(fs.readFileSync(artifact, 'utf8'), browser, { filename: artifact });
check(browser.Mldoc);
console.log('installed JS export (Node and browser context): OK');
