# TCL File Generated by Component Editor 18.1
# Tue Dec 17 17:58:48 CET 2024
# DO NOT MODIFY


# 
# anomaly_detection "Anomaly Detection" v1.0
# Giuseppe Webber 2024.12.17.17:58:48
# 
# 

# 
# request TCL package from ACDS 16.1
# 
package require -exact qsys 16.1


# 
# module anomaly_detection
# 
set_module_property DESCRIPTION ""
set_module_property NAME anomaly_detection
set_module_property VERSION 1.0
set_module_property INTERNAL false
set_module_property OPAQUE_ADDRESS_MAP true
set_module_property GROUP "My IP Cores"
set_module_property AUTHOR "Giuseppe Webber"
set_module_property DISPLAY_NAME "Anomaly Detection"
set_module_property INSTANTIATE_IN_SYSTEM_MODULE true
set_module_property EDITABLE true
set_module_property REPORT_TO_TALKBACK false
set_module_property ALLOW_GREYBOX_GENERATION false
set_module_property REPORT_HIERARCHY false


# 
# file sets
# 
add_fileset QUARTUS_SYNTH QUARTUS_SYNTH "" ""
set_fileset_property QUARTUS_SYNTH TOP_LEVEL anomaly_detection
set_fileset_property QUARTUS_SYNTH ENABLE_RELATIVE_INCLUDE_PATHS false
set_fileset_property QUARTUS_SYNTH ENABLE_FILE_OVERWRITE_MODE false
add_fileset_file anomaly_detection.vhd VHDL PATH ../../../anomaly_detection/anomaly_detection.vhd TOP_LEVEL_FILE

add_fileset SIM_VHDL SIM_VHDL "" ""
set_fileset_property SIM_VHDL TOP_LEVEL anomaly_detection
set_fileset_property SIM_VHDL ENABLE_RELATIVE_INCLUDE_PATHS false
set_fileset_property SIM_VHDL ENABLE_FILE_OVERWRITE_MODE false
add_fileset_file anomaly_detection.vhd VHDL PATH ../../../anomaly_detection/anomaly_detection.vhd


# 
# parameters
# 
add_parameter IM_SIZE INTEGER 500
set_parameter_property IM_SIZE DEFAULT_VALUE 500
set_parameter_property IM_SIZE DISPLAY_NAME IM_SIZE
set_parameter_property IM_SIZE TYPE INTEGER
set_parameter_property IM_SIZE UNITS None
set_parameter_property IM_SIZE ALLOWED_RANGES -2147483648:2147483647
set_parameter_property IM_SIZE HDL_PARAMETER true
add_parameter ADDR_SIZE INTEGER 16
set_parameter_property ADDR_SIZE DEFAULT_VALUE 16
set_parameter_property ADDR_SIZE DISPLAY_NAME ADDR_SIZE
set_parameter_property ADDR_SIZE TYPE INTEGER
set_parameter_property ADDR_SIZE UNITS None
set_parameter_property ADDR_SIZE ALLOWED_RANGES -2147483648:2147483647
set_parameter_property ADDR_SIZE HDL_PARAMETER true
add_parameter WORD_SIZE INTEGER 16
set_parameter_property WORD_SIZE DEFAULT_VALUE 16
set_parameter_property WORD_SIZE DISPLAY_NAME WORD_SIZE
set_parameter_property WORD_SIZE TYPE INTEGER
set_parameter_property WORD_SIZE UNITS None
set_parameter_property WORD_SIZE ALLOWED_RANGES -2147483648:2147483647
set_parameter_property WORD_SIZE HDL_PARAMETER true
add_parameter COUNT_WIDTH INTEGER 9
set_parameter_property COUNT_WIDTH DEFAULT_VALUE 9
set_parameter_property COUNT_WIDTH DISPLAY_NAME COUNT_WIDTH
set_parameter_property COUNT_WIDTH TYPE INTEGER
set_parameter_property COUNT_WIDTH UNITS None
set_parameter_property COUNT_WIDTH ALLOWED_RANGES -2147483648:2147483647
set_parameter_property COUNT_WIDTH HDL_PARAMETER true


# 
# display items
# 


# 
# connection point clock_sink
# 
add_interface clock_sink clock end
set_interface_property clock_sink clockRate 0
set_interface_property clock_sink ENABLED true
set_interface_property clock_sink EXPORT_OF ""
set_interface_property clock_sink PORT_NAME_MAP ""
set_interface_property clock_sink CMSIS_SVD_VARIABLES ""
set_interface_property clock_sink SVD_ADDRESS_GROUP ""

add_interface_port clock_sink clk clk Input 1


# 
# connection point clock_reset
# 
add_interface clock_reset reset end
set_interface_property clock_reset associatedClock clock_sink
set_interface_property clock_reset synchronousEdges DEASSERT
set_interface_property clock_reset ENABLED true
set_interface_property clock_reset EXPORT_OF ""
set_interface_property clock_reset PORT_NAME_MAP ""
set_interface_property clock_reset CMSIS_SVD_VARIABLES ""
set_interface_property clock_reset SVD_ADDRESS_GROUP ""

add_interface_port clock_reset rst reset Input 1


# 
# connection point avalon_streaming_source
# 
add_interface avalon_streaming_source avalon_streaming start
set_interface_property avalon_streaming_source associatedClock clock_sink
set_interface_property avalon_streaming_source associatedReset clock_reset
set_interface_property avalon_streaming_source dataBitsPerSymbol 16
set_interface_property avalon_streaming_source errorDescriptor ""
set_interface_property avalon_streaming_source firstSymbolInHighOrderBits true
set_interface_property avalon_streaming_source maxChannel 0
set_interface_property avalon_streaming_source readyLatency 0
set_interface_property avalon_streaming_source ENABLED true
set_interface_property avalon_streaming_source EXPORT_OF ""
set_interface_property avalon_streaming_source PORT_NAME_MAP ""
set_interface_property avalon_streaming_source CMSIS_SVD_VARIABLES ""
set_interface_property avalon_streaming_source SVD_ADDRESS_GROUP ""

add_interface_port avalon_streaming_source data_out data Output word_size
add_interface_port avalon_streaming_source endpacket_out endofpacket Output 1
add_interface_port avalon_streaming_source ready_in ready Input 1
add_interface_port avalon_streaming_source startpacket_out startofpacket Output 1
add_interface_port avalon_streaming_source valid_out valid Output 1


# 
# connection point avalon_streaming_sink
# 
add_interface avalon_streaming_sink avalon_streaming end
set_interface_property avalon_streaming_sink associatedClock clock_sink
set_interface_property avalon_streaming_sink associatedReset clock_reset
set_interface_property avalon_streaming_sink dataBitsPerSymbol 16
set_interface_property avalon_streaming_sink errorDescriptor ""
set_interface_property avalon_streaming_sink firstSymbolInHighOrderBits true
set_interface_property avalon_streaming_sink maxChannel 0
set_interface_property avalon_streaming_sink readyLatency 0
set_interface_property avalon_streaming_sink ENABLED true
set_interface_property avalon_streaming_sink EXPORT_OF ""
set_interface_property avalon_streaming_sink PORT_NAME_MAP ""
set_interface_property avalon_streaming_sink CMSIS_SVD_VARIABLES ""
set_interface_property avalon_streaming_sink SVD_ADDRESS_GROUP ""

add_interface_port avalon_streaming_sink data_in data Input word_size
add_interface_port avalon_streaming_sink endpacket_in endofpacket Input 1
add_interface_port avalon_streaming_sink ready_out ready Output 1
add_interface_port avalon_streaming_sink startpacket_in startofpacket Input 1
add_interface_port avalon_streaming_sink valid_in valid Input 1

