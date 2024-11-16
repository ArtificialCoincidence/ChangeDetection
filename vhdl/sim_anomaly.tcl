vcom.exe -2008 anomaly/bram_histogram.vhd
vcom.exe -2008 anomaly/histogram.vhd
vcom.exe -2008 anomaly/threshold_cdf.vhd
vcom.exe -2008 anomaly/scan_image.vhd
vcom.exe -2008 anomaly/anomaly.vhd
vcom.exe -2008 tb/read_data.vhd
vcom.exe -2008 tb/tb_anomaly.vhd

vsim.exe -t 10ps work.tb_anomaly -voptargs=+acc
