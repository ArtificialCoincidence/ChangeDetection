library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity anomaly_detection_tb is
end entity anomaly_detection_tb;

architecture tb of anomaly_detection_tb is
    -- Constants
    constant CLK_PERIOD : time := 10 ns;

    -- DUT (Device Under Test) Signals
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
    -- DUT Instantiation
    uut: entity work.anomaly_detection
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

    -- Clock Generation
    clk_process: process
    begin
        clk <= '0';
        wait for CLK_PERIOD / 2;
        clk <= '1';
        wait for CLK_PERIOD / 2;
    end process clk_process;

    -- Stimulus Process
    stimulus_process: process
    begin
        -- Reset the DUT
        rst <= '1';
        wait for CLK_PERIOD * 2;
        rst <= '0';

        -- Test Case 1: Simple Packet Transmission
        wait for CLK_PERIOD;
        startpacket_in <= '1';
        valid_in <= '1';
        data_in <= x"0001";
        wait for CLK_PERIOD;
        startpacket_in <= '0';
        data_in <= x"0002";
        wait for CLK_PERIOD;
        endpacket_in <= '1';
        data_in <= x"0003";
        wait for CLK_PERIOD;
        endpacket_in <= '0';
        valid_in <= '0';

        -- Wait for processing
        wait for CLK_PERIOD * 10;

        -- Add more test cases as needed

        -- Finish Simulation
        wait;
    end process stimulus_process;

end architecture tb;
