library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;
use ieee.fixed_pkg.all;

entity tb_anomaly is
end tb_anomaly;

architecture test of tb_anomaly is

    component anomaly is
        generic(
            IM_SIZE     : integer := 500;
            ADDR_SIZE   : integer := 16;
            WORD_SIZE   : integer := 16
        );
        port (
            clk 		: in  std_logic;
            rst			: in  std_logic;	
            data_valid	: in  std_logic;
            img_data_i	: in  std_logic_vector(WORD_SIZE-1 downto 0); -- pixel data in
            img_data_o	: out std_logic_vector(WORD_SIZE-1 downto 0) -- pixel data out
        );
    end component;

    component read_data is
        generic(
            file_path   : string := "";
            ENTRIES		: integer := 500;
            WORD_SIZE	: integer := 16;
            ADDR_SIZE	: integer := 16
        );
        port(
            clk         : in  std_logic;
            rst         : in  std_logic;
            addr		: in  std_logic_vector(ADDR_SIZE-1 downto 0);
            enable		: in  std_logic;
            data_out	: out std_logic_vector(ADDR_SIZE-1 downto 0)
        );
    end component;

    constant WORD_SIZE  : integer := 8;
    constant ADDR_SIZE  : integer := 8;
    constant IM_SIZE    : integer := 100;

    signal clk_tb       : std_logic := '1';
    signal rst_tb       : std_logic := '0';
    signal data_in_tb   : std_logic_vector(WORD_SIZE-1 downto 0); --ufixed(-1 downto -32);
    signal enable       : std_logic;
    signal data_valid   : std_logic;
    signal data_out_tb  : std_logic_vector(WORD_SIZE-1 downto 0); --ufixed(-1 downto -32);
    signal address      : std_logic_vector(WORD_SIZE-1 downto 0);
  
begin
  
  dut: anomaly
    generic map(
        IM_SIZE         => IM_SIZE,
        ADDR_SIZE       => ADDR_SIZE,
        WORD_SIZE       => WORD_SIZE
    )
    port map(
        clk 		    => clk_tb,
        rst			    => rst_tb,
        data_valid	    => data_valid,
        img_data_i	    => data_in_tb,
        img_data_o      => data_out_tb
    );

  dmem: read_data
    generic map(
        file_path       => "tb/files/AROutputs/AR1_Outputs_NoChanges/St_inref1.txt",
        ENTRIES         => IM_SIZE,
        WORD_SIZE       => WORD_SIZE,
        ADDR_SIZE       => ADDR_SIZE
    )
    port map(
        clk             => clk_tb,
        rst             => rst_tb,
        addr            => address,
        enable          => enable,
        data_out        => data_in_tb
    );

    clk_tb <= not clk_tb after 0.5 ns;

    -- PROCESS FOR TESTING
    stim: process
    begin
        rst_tb <= '1', '0' after 10 ns; 
        enable <= '0', '1' after 10 ns;
        data_valid <= '0', '1' after 10 ns;
        address <= (others => '0');
        wait for 10.5 ns;

        for i in 0 to IM_SIZE-2 loop
            address <= std_logic_vector(unsigned(address) + 1);
            wait for 1 ns;
        end loop;
        
        address <= (others => '0');
        rst_tb <= '1', '0' after 1 ns;
        data_valid <= '0';
        wait for 1.5 ns;
        
        for i in 0 to IM_SIZE-2 loop
            address <= std_logic_vector(unsigned(address) + 1);
            wait for 1 ns;
        end loop;
        wait;

    end process stim;
end test;
