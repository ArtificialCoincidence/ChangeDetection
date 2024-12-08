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
		addr_gen				: out std_logic_vector(COUNT_WIDTH-1 downto 0)
	);
end control_fsm;

architecture rtl of control_fsm is

  type 	state_type is (IDLE, RECEIVE, THRESHOLD, SCAN, HOLD);
  signal state, next_state : state_type := IDLE;

  signal counter_receive 	: unsigned(COUNT_WIDTH-1 downto 0) := (others => '0');
  signal counter_scan    	: unsigned(COUNT_WIDTH-1 downto 0) := (others => '0');
  signal en_cnt1 				: std_logic_vector(1 downto 0) := "00";
  signal en_cnt2 				: std_logic_vector(1 downto 0) := "00";

begin
	
	-- Receive Counter
    process(clk, rst)
    begin
        if rst = '1' then
            counter_receive <= (others => '0');
        elsif rising_edge(clk) then
            case en_cnt1 is
                when "00" =>
                    counter_receive <= (others => '0');
                when "01" =>
                    counter_receive <= counter_receive + 1;
                when others =>
                    counter_receive <= counter_receive;
            end case;
        end if;
    end process;

    -- Scan Counter
    process(clk, rst)
    begin
        if rst = '1' then
            counter_scan <= (others => '0');
        elsif rising_edge(clk) then
            case en_cnt2 is
                when "00" =>
                    counter_scan <= (others => '0');
                when "01" =>
                    counter_scan <= counter_scan + 1;
                when others =>
                    counter_scan <= counter_scan;
            end case;
        end if;
    end process;

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
	process(state, valid_in, ready_in, counter_receive, counter_scan, thrs_sig)
	begin
		case state is
			when IDLE =>
				if valid_in = '1' then
					next_state <= RECEIVE;
				else
					next_state <= IDLE;
				end if;

			when RECEIVE =>
				if valid_in = '0' then
					next_state <= IDLE;
				elsif counter_receive < 500 then
					next_state <= RECEIVE;
				elsif counter_receive = 500 then
					next_state <= THRESHOLD;
				end if;

			when THRESHOLD =>
				if thrs_sig = '1' then
					next_state <= SCAN;
				else
					next_state <= THRESHOLD;
				end if;

			when SCAN =>
				if ready_in = '0' then
					next_state <= HOLD;
				elsif counter_scan < 500 then
					next_state <= SCAN;
				elsif counter_scan = 500 then
					next_state <= IDLE;
				end if;
			
			when HOLD =>
				if ready_in = '0' then
					next_state <= HOLD;
				else
					next_state <= SCAN;
				end if;

			when others =>
				next_state <= IDLE;
		end case;
	end process;	 

	-- Output Logic
	process(state, counter_receive, counter_scan)
		begin
			-- Default values
			addr_gen <= (others => '0');
			ready_out <= '0';
			thrs_en <= '0';
			hist_en <= '0';
			hist_rst <= '0';
			bram_rd_en <= '0';
			bram_wr_en <= '0';
			scan_en <= '0';
			valid_out <= '0';
			startpacket_out <= '0';
			endpacket_out <= '0';
			en_cnt1 <= "00";
			en_cnt2 <= "00";

        case state is
            when IDLE =>
                ready_out <= '0';
            when RECEIVE =>
                addr_gen <= std_logic_vector(counter_receive);
                en_cnt1 <= "01";
                ready_out <= '1';
                hist_en <= '1';
                bram_wr_en <= '1';
                scan_en <= '1';
            when THRESHOLD =>
                thrs_en <= '1';
            when SCAN =>
                addr_gen <= std_logic_vector(counter_scan);
                en_cnt2 <= "01";
                scan_en <= '1';
					 bram_rd_en <= '1';
                valid_out <= '1';
					 hist_rst <= '1';
                if counter_scan = 1 then
                    startpacket_out <= '1';
                end if;
                if counter_scan = 500 then
                    endpacket_out <= '1';
                end if;
            when HOLD =>
                en_cnt1 <= "10";
                en_cnt2 <= "10";
                scan_en <= '1';
            when others =>
                null;
        end case;
    end process;

end architecture rtl;
