import { useState } from "react";
import { invoke } from "@tauri-apps/api/core";
import { open } from "@tauri-apps/plugin-dialog";
import "./App.css";

function App() {
  const [resultMsg, setResultMsg] = useState("");
  const [filePath, setFilePath] = useState("");

  async function selectAndProcessFile() {
    try {
      const selectedPath = await open({
        title: "Selecione o arquivo para processar",
        filters: [{ name: 'RCL', extensions: ['rcl'] }],
      });

      if (selectedPath) {
        const pathString = Array.isArray(selectedPath) ? selectedPath[0] : selectedPath;
        setFilePath(pathString);

        const response = await invoke("process_file", { path: pathString });

        setResultMsg(String(response));
      } else {
        setFilePath("");
        setResultMsg("No file selected.");
      }
    } catch (error) {
      console.error("Erro ao selecionar ou processar arquivo:", error);
      setResultMsg(`Erro: ${error}`);
    }
  }

  return (
    <main className="container">
      <h1>Welcome to Recall</h1>

      <div className="row">
        <button onClick={selectAndProcessFile}>
          Select and Process File
        </button>
      </div>

      {filePath && <p>Selected file: <strong>{filePath}</strong></p>}
      <p style={{ marginTop: "10px" }}>Result: {resultMsg}</p>
    </main>
  );
}

export default App;
