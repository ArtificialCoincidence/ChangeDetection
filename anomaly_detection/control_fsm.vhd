library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity control_fsm is
	generic(
		COUNT_WIDTH : integer := 9
	);
	port(
		clk 					: in  std_logic;
		rst					: in  std_logic;
		-- SINK
		ready_out			: out std_logic;
		startpacket_in		: in  std_logic;
		endpacket_in		: in  std_logic;
		valid_in				: in  std_logic;
		-- SOURCE
		ready_in				: in  std_logic;
		startpacket_out	: out std_logic;
		endpacket_out		: out std_logic;
		valid_out			: out std_logic;
		-- ENABLES
		hist_en				: out std_logic;
		bram_rd_en			: out std_logic;
		bram_wr_en			: out std_logic;
		thrs_en				: out std_logic;
		scan_en				: out std_logic;
		-- RESET
		hist_rst				: out std_logic;
		thrs_sig				: in  std_logic;
		-- BRAM ADDR
		addr_gen				: out std_logic_vector(COUNT_WIDTH-1 downto 0);
	);
end control_fsm;

architecture rtl of control_fsm is

  type 	state_type is (IDLE, RECEIVE, THRESHOLD, SCAN);
  signal state, next_state : state_type;

  signal counter_receive 	: unsigned(COUNT_WIDTH-1 downto 0) := (others => '0');
  signal counter_scan    	: unsigned(COUNT_WIDTH-1 downto 0) := (others => '0');

begin

	-- State Register
	process(clk, rst)
	begin
		if rst = '1' then
			state <= IDLE;
		elsif rising_edge(clk) then
			state <= next_state;
		end if;
	end process;

	-- Next State Logic
	process(state, valid_in, counter_receive, counter_scan, thrs_sig)
	begin
		case state is
			when IDLE =>
				if valid_in = '1' then
					next_state <= RECEIVE;
				else
					next_state <= IDLE;
				end if;

			when RECEIVE =>
				if counter_receive < 500 then
					next_state <= RECEIVE;
				elsif counter_receive = 500 then
					next_state <= THRESHOLD;
				end if;

			when THRESHOLD =>
				if THRESHOLD = '1' then
					next_state <= SCAN;
				else
					next_state <= THRESHOLD;
				end if;

			when SCAN =>
				if counter_scan < 500 then
					next_state <= SCAN;
				elsif counter_scan = 500 then
					next_state <= IDLE;
				end if;

			when others =>
				next_state <= IDLE;
		end case;
	end process;
	
	
	
	
	-- to fix: output logic -> manage ready, valid, start and stop
	--			  counters     -> manage in a separate process? 

	-- Output Logic and Counter Updates
	process(state, counter_receive, counter_scan)
	begin
		-- Default values
    READY <= '0';
    EN_TRHES <= '0';
    EN_HIST <= '0';
    EN_BRAM <= '0';
    EN_SCAN <= '0';
    WRADRR <= (others => '0');
    RDADRR <= (others => '0');

    case state is
      when IDLE =>
        counter_receive <= (others => '0');
        counter_scan <= (others => '0');
        READY <= '1';

      when RECEIVE =>
        counter_receive <= counter_receive + 1;
        WRADRR <= std_logic_vector(counter_receive);
        READY <= '1';
        EN_HIST <= '1';
        EN_BRAM <= '1';
        EN_SCAN <= '1';

      when THRESHOLD =>
        EN_TRHES <= '1';

      when SCAN =>
        counter_scan <= counter_scan + 1;
        RDADRR <= std_logic_vector(counter_scan);
        EN_SCAN <= '1';

      when others =>
        -- Do nothing
        null;
    end case;
  end process;

end architecture Behavioral;
