import { Variable } from "astal";
import { subprocess } from "astal/process";
import { configPath } from "../build";
import { PercentProgress, Section } from "./components";
import { Gtk } from "astal/gtk3";

export const run_subprocess = (command: string, callback: (output: string) => void) => {
  return subprocess({
    cmd: command,
    out: (data) => callback(data),
  });
};

// Define interfaces for system metrics
interface NetworkMetrics {
  upload_kbps: number;
  download_kbps: number;
}

interface CpuMetrics {
  percent: number;
  temperature?: number;
  frequency?: {
    current: number;
    min?: number;
    max?: number;
  };
}

interface MemoryMetrics {
  percent: number;
  available_gb: number;
  total_gb: number;
}

interface SystemMetrics {
  cpu: CpuMetrics;
  memory: MemoryMetrics;
  network: NetworkMetrics;
}

// Create reactive variables for system metrics
export const systemMetrics = new Variable<SystemMetrics>({
  cpu: { percent: 0 },
  memory: { percent: 0, available_gb: 0, total_gb: 0 },
  network: { upload_kbps: 0, download_kbps: 0 },
});

// Start the CPU monitor subprocess
const startCpuMonitor = () => {
  const scriptPath = `${configPath}/cpu_monitor.py`;

  return run_subprocess(scriptPath, (output) => {
    try {
      const data = JSON.parse(output);
      systemMetrics.set(data);
    } catch (error) {
      console.error("Failed to parse CPU monitor output:", error);
    }
  });
};

// Initialize CPU monitor when imported
startCpuMonitor();

const CpuStatus = () => {
  return (
    <Section name="CpuStatus" className="monospace" icon={"computer"}>
      <PercentProgress value={systemMetrics((m) => m.cpu.percent / 100)} />
      <label
        label={systemMetrics(
          (m) =>
            `${m.cpu.temperature ? ` ${m.cpu.temperature.toFixed(0)}°` : ""}${
              m.cpu.frequency ? ` ${m.cpu.frequency.current.toFixed(2)}` : ""
            }`,
        )}
      />
      {systemMetrics((m) => m.cpu.frequency && <label className="unit_label" label="GHz" />)}
    </Section>
  );
};

const MemoryStatus = () => {
  const vals = systemMetrics((m) => {
    if (!m) return { used_gb: 0, total_gb: 0, used_percent: 0, used_value: 0 };
    const available_gb = m.memory.available_gb;
    const total_gb = m.memory.total_gb;
    const used_gb = total_gb - available_gb;
    const used_value = used_gb / total_gb;
    const used_percent = Math.round(used_value * 100);
    return { used_gb, total_gb, used_percent, used_value };
  });

  return (
    <Section
      name="MemoryStatus"
      className="monospace"
      visible={systemMetrics((m) => !!m)}
      icon={"memory"}
    >
      <PercentProgress value={vals.as((v) => v.used_value)} className={"MemoryProgress"} />
      <label className={"used_mem"} label={vals.as((v) => `${v.used_gb.toFixed(1)}`)} />
      <label className={"slash_label"} label={"/"} />
      <label className={"total_mem"} label={vals.as((v) => `${v.total_gb.toFixed(0)}`)} />
      <label className={"gb_label"} label={"gb"} />
    </Section>
  );
};

// System status component
const SystemStatus = () => (
  <box className="SystemStatus" vexpand valign={Gtk.Align.CENTER}>
    <CpuStatus />
    <MemoryStatus />
  </box>
);

export default SystemStatus;
