import Stdio from 'std/io/stdio';
let stderr = Stdio.stderr;

stderr.write('expected 1');
stderr.write(' expected 2');
stderr.flush();