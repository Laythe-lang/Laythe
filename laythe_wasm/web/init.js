import { Terminal } from "xterm";
import { FitAddon } from "xterm-addon-fit";
import { languages, editor } from "monaco-editor";
import { language, conf } from "./language";

const id = "laythe";

export function terminalInit(element, version) {
  const term = new Terminal({
    rendererType: "canvas",
    fontSize: 15,
    cursorBlink: true,
  });
  const fitAddon = new FitAddon();
  term.loadAddon(fitAddon);
  term.open(element);

  fitAddon.fit();

  term.writeln("Welcome to the Laythe playground!");
  term.writeln(`You are currently running Laythe version ${version}`);
  term.writeln(
    "Enter a script in the left and press run to see the output here."
  );

  return term;
}

export function editorInit(element) {
  languages.register({
    id,
    extensions: [".ly"],
    aliases: ["Laythe", "laythe"],
  });

  languages.setMonarchTokensProvider(id, language);
  languages.onLanguage(id, () => languages.setLanguageConfiguration(id, conf));

  editor.create(element, {
    value: ["fn hi() {", '  print("hello world");', "}", "", "hi();"].join("\n"),
    automaticLayout: true,
    language: "laythe",
  });

  return editor;
}