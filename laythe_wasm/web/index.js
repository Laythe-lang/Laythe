import { VmWasm } from "laythe";
import { terminalInit, editorInit } from "./init";

const runElement = document.getElementById("run");
const replElement = document.getElementById("repl");

const editorElement = document.getElementById("editor");
const terminalElement = document.getElementById("terminal");

let repl = false;
let curr_line = "";

let timeoutHandle;
const editor = editorInit(editorElement);

const term = terminalInit(terminalElement, VmWasm.version());

const runScript = () => {
  repl = false;
  clearTimeout(timeoutHandle);
  const [model] = editor.getModels();
  const source = model.getValue();

  term.clear();
  const vm = VmWasm.with_stdout((str) => term.writeln(str));

  setTimeout(() => vm.run(source));
};

const startRepl = () => {
  if (!repl) {
    repl = true;

    clearTimeout(timeoutHandle);
    term.clear();
    setTimeout(() => term.write("> "));
  }
};

timeoutHandle = setTimeout(startRepl, 5000);

// VM for the repl
const replVm = VmWasm.with_stdout((str) => {
  term.writeln(str);
});

// on run strip source from editor and pipe it through a new vm
runElement.addEventListener("click", runScript);

// on repl clear terminal and set repl to true
replElement.addEventListener("click", startRepl);

term.onKey(({ key, domEvent: { keyCode } }) => {
  if (repl) {
    console.log(key, keyCode);

    switch (keyCode) {
      case 13:
        term.write("\r\n");
        replVm.run(curr_line);
        term.write("\r\n> ");
        curr_line = "";
        break;
      case 8:
        if (curr_line) {
          curr_line = curr_line.slice(0, curr_line.length - 1);
          term.write("\b \b");
        }
        break;
      default:
        curr_line += key;
        term.write(key);
    }
  }
});

term.attachCustomKeyEventHandler((event) => {
  console.log(event);
  return true;
});
