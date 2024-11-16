vcom.exe anomaly/bram_histogram.vhd
vcom.exe anomaly/histogram.vhd
vcom.exe anomaly/threshold_cdf.vhd
vcom.exe anomaly/scan_image.vhd
vcom.exe anomaly/anomaly.vhd
vcom.exe tb/read_data.vhd
vcom.exe tb/tb_anomaly.vhd

vsim.exe -t 10ps work.tb_anomaly -voptargs=+acc
