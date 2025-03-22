#!/usr/bin/env python3
import psutil
import time
import sys
import json


def get_network_speed():
    # Get initial network counters
    net_io_counters_start = psutil.net_io_counters()

    # Wait for a short period to calculate speed
    time.sleep(1)

    # Get network counters after the interval
    net_io_counters_end = psutil.net_io_counters()

    # Calculate network speeds in KB/s
    bytes_sent = net_io_counters_end.bytes_sent - net_io_counters_start.bytes_sent
    bytes_recv = net_io_counters_end.bytes_recv - net_io_counters_start.bytes_recv

    # Convert to KB/s
    kb_sent = bytes_sent / 1024
    kb_recv = bytes_recv / 1024

    return {"upload_kbps": round(kb_sent, 1), "download_kbps": round(kb_recv, 1)}


def get_cpu_temperature():
    try:
        # Check if we can get temperatures
        if not hasattr(psutil, "sensors_temperatures"):
            return None

        # Get all temperatures
        temps = psutil.sensors_temperatures()

        # Look for k10temp temperature sensor
        if "k10temp" in temps:
            for entry in temps["k10temp"]:
                if entry.label == "Tctl":
                    return entry.current

        return None
    except Exception as e:
        print(f"Error getting temperature: {e}", file=sys.stderr)
        return None


def get_cpu_frequency():
    try:
        freq = psutil.cpu_freq()
        if freq:
            return {
                "current": round(freq.current / 1000, 2),  # Convert MHz to GHz
                "min": round(freq.min / 1000, 2) if freq.min else None,
                "max": round(freq.max / 1000, 2) if freq.max else None
            }
        return None
    except Exception as e:
        print(f"Error getting CPU frequency: {e}", file=sys.stderr)
        return None


def main():
    try:
        # For first iteration, we need initial CPU measurement
        psutil.cpu_percent()

        while True:
            # Get network speed
            # This function already includes a 1-second delay
            network_speed = get_network_speed()

            # Get CPU usage as a percentage
            cpu_percent = psutil.cpu_percent()

            # Get CPU temperature
            cpu_temp = get_cpu_temperature()

            # Get CPU frequency
            cpu_freq = get_cpu_frequency()

            # Get memory usage
            memory = psutil.virtual_memory()
            memory_available_gb = memory.available / (1024 * 1024 * 1024)
            memory_total_gb = memory.total / (1024 * 1024 * 1024)

            # Calculate percent used from available and total
            memory_used_percent = 100 - (memory_available_gb / memory_total_gb * 100)

            # Create CPU metrics dict
            cpu_metrics = {"percent": round(cpu_percent, 1)}

            # Add temperature if available
            if cpu_temp is not None:
                cpu_metrics["temperature"] = round(cpu_temp, 1)

            # Add frequency if available
            if cpu_freq is not None:
                cpu_metrics["frequency"] = cpu_freq

            # Create JSON object with system metrics
            metrics = {
                "cpu": cpu_metrics,
                "memory": {
                    "percent": round(memory_used_percent, 1),
                    "available_gb": round(memory_available_gb, 1),
                    "total_gb": round(memory_total_gb, 1),
                },
                "network": network_speed,
            }

            # Print JSON to stdout and flush immediately
            print(json.dumps(metrics), flush=True)

    except KeyboardInterrupt:
        # Exit gracefully on Ctrl+C
        sys.exit(0)


if __name__ == "__main__":
    main()
