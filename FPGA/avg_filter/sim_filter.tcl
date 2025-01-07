vcom.exe avg_filter.vhd
vcom.exe read_data.vhd
vcom.exe tb/TB_avg_filter.vhd

vsim.exe -t 10ps work.tb_avg_filter -voptargs=+acc
add wave *
run 100 ns
