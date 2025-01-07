library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity anomaly_tb is
end entity anomaly_tb;

architecture tb of anomaly_tb is

    component anomaly_detection is
        generic(
            IM_SIZE 	: integer := 500;
            ADDR_SIZE 	: integer := 16;
            WORD_SIZE 	: integer := 16;
            COUNT_WIDTH	: integer := 9
        );
        port (
            clk 					: in  std_logic;
            rst					: in  std_logic;
            -- SINK
            ready_out			: out std_logic;
            data_in				: in  std_logic_vector(WORD_SIZE-1 downto 0);
            startpacket_in		: in  std_logic;
            endpacket_in		: in  std_logic;
            valid_in				: in  std_logic;
            -- SOURCE
            ready_in				: in  std_logic;
            data_out				: out std_logic_vector(WORD_SIZE-1 downto 0);
				startpacket_out	: out std_logic;
            endpacket_out		: out std_logic;
            valid_out			: out std_logic
        );
    end component;

    component read_data is
        generic(
            file_path       	: string := "";
            ENTRIES		    	: integer := 500;
            WORD_SIZE	    	: integer := 16;
            ADDR_SIZE	    	: integer := 16
        );
        port(
            clk             	: in  std_logic;
            rst             	: in  std_logic;
            enable		    	: in  std_logic;
            data_out	    	 	: out std_logic_vector(ADDR_SIZE-1 downto 0);
				valid					: out std_logic;
				endpacket			: out std_logic;
				startpacket			: out std_logic
        );
    end component;

    component write_data is
        generic(
            file_path   		: string := "";
            ENTRIES				: integer := 500;
            WORD_SIZE			: integer := 16;
            ADDR_SIZE			: integer := 16
            );
        port (
            clk					: in std_logic;
            rst             	: in std_logic;
            enable				: in std_logic;
            data_in         	: in std_logic_vector(WORD_SIZE-1 downto 0);
				ready					: out std_logic
        );
    end component;
    
    constant CLK_PERIOD : time := 1 ns;

    signal clk             : std_logic := '0';
    signal rst             : std_logic := '0';
    signal ready_out       : std_logic;
    signal data_in         : std_logic_vector(15 downto 0) := (others => '0');
    signal startpacket_in  : std_logic := '0';
    signal endpacket_in    : std_logic := '0';
    signal valid_in        : std_logic := '0';
    signal ready_in        : std_logic := '0';
    signal data_out        : std_logic_vector(15 downto 0);
    signal startpacket_out : std_logic;
    signal endpacket_out   : std_logic;
    signal valid_out       : std_logic;

begin
    
    dut: anomaly_detection
    generic map (
        IM_SIZE      => 500,
        ADDR_SIZE    => 16,
        WORD_SIZE    => 16,
        COUNT_WIDTH  => 9
    )
    port map (
        clk             => clk,
        rst             => rst,
        ready_out       => ready_out,
        data_in         => data_in,
        startpacket_in  => startpacket_in,
        endpacket_in    => endpacket_in,
        valid_in        => valid_in,
        ready_in        => ready_in,
        data_out        => data_out,
        startpacket_out => startpacket_out,
        endpacket_out   => endpacket_out,
        valid_out       => valid_out
    );

    rmem: read_data
    generic map(
        file_path       => "../../tb/files/AROutputs/AR1_Outputs_Changes/Sc_inref3.txt",
        ENTRIES         => 500,
        WORD_SIZE       => 16,
        ADDR_SIZE       => 16
    )
    port map(
        clk             => clk,
        rst             => rst,
        enable          => ready_out,
        data_out        => data_in,
		  valid				=> valid_in,
		  endpacket			=> endpacket_in,
		  startpacket		=> startpacket_in
    );

    wmem: write_data
    generic map(
        file_path       => "../../tb/files/anomalyOutputs/anomaly_Outputs_Changes/Sc_inref3.txt",
        ENTRIES         => 500,
        WORD_SIZE       => 16,
        ADDR_SIZE       => 16
    )
    port map(
        clk             => clk,
        rst             => rst,
        enable          => valid_out,
        data_in         => data_out,
		  ready 				=> ready_in
    );

    clk_process: process
    begin
        clk <= '0';
        wait for CLK_PERIOD / 2;
        clk <= '1';
        wait for CLK_PERIOD / 2;
    end process clk_process;

    stim_process: process
    begin
        rst <= '1';
        wait for CLK_PERIOD * 2;
        rst <= '0';

        wait;
    end process stim_process;

end architecture tb;
