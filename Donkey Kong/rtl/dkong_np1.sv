/*
  
   Multicore 2 / Multicore 2+
  
   Copyright (c) 2017-2020 - Victor Trucco

  
   All rights reserved
  
   Redistribution and use in source and synthezised forms, with or without
   modification, are permitted provided that the following conditions are met:
  
   Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.
  
   Redistributions in synthesized form must reproduce the above copyright
   notice, this list of conditions and the following disclaimer in the
   documentation and/or other materials provided with the distribution.
  
   Neither the name of the author nor the names of other contributors may
   be used to endorse or promote products derived from this software without
   specific prior written permission.
  
   THIS CODE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
   AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
   THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
   PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE
   LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
   CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
   SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
   INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
   CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
   ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
   POSSIBILITY OF SUCH DAMAGE.
  
   You are responsible for any legal issues arising from your use of this code.
*/
//============================================================================
//
//  Multicore 2+ Top by Victor Trucco
//
//============================================================================
//
//============================================================================
//
//  neptUNO adapted by Delgrom
//
//============================================================================
`default_nettype none

module dkong_np1(
   // Clocks
    input wire  clock_50_i,

    // Buttons
    //input wire [4:1]    btn_n_i,

    // SRAM (IS61WV102416BLL-10TLI)
    output wire [19:0]sram_addr_o  = 20'b00000000000000000000,
    inout wire  [15:0]sram_data_io   = 8'bzzzzzzzzbzzzzzzzz,
    output wire sram_we_n_o     = 1'b1,
    output wire sram_oe_n_o     = 1'b1,
    output wire sram_ub_n_o     = 1'b1,
    output wire sram_lb_n_o     = 1'b1,	 
	         
    // SDRAM    (H57V256)
    output [12:0] SDRAM_A,
    output  [1:0] SDRAM_BA,
    inout  [15:0] SDRAM_DQ,
    output        SDRAM_DQMH,
    output        SDRAM_DQML,
    output        SDRAM_CKE,
    output        SDRAM_nCS,
    output        SDRAM_nWE,
    output        SDRAM_nRAS,
    output        SDRAM_nCAS,
    output        SDRAM_CLK,

    // PS2
    inout wire  ps2_clk_io          = 1'bz,
    inout wire  ps2_data_io         = 1'bz,
    inout wire  ps2_mouse_clk_io  = 1'bz,
    inout wire  ps2_mouse_data_io = 1'bz,

    // SD Card
    output wire sd_cs_n_o           = 1'b1,
    output wire sd_sclk_o           = 1'b0,
    output wire sd_mosi_o           = 1'b0,
    input wire  sd_miso_i,

    // Joysticks
    output wire joy_clock_o         = 1'b1,
    output wire joy_load_o          = 1'b1,
    input  wire joy_data_i,
    output wire joy_p7_o            = 1'b1,

    // Audio
    output        AUDIO_L,
    output        AUDIO_R,
    input wire  ear_i,
    //output wire mic_o                   = 1'b0,
    //  I2S
    output		SCLK,
    output		LRCLK,
    output		SDIN,		 

        // VGA
    output  [5:0] VGA_R,
    output  [5:0] VGA_G,
    output  [5:0] VGA_B,
    output        VGA_HS,
    output        VGA_VS,

        //STM32
    input wire  stm_tx_i,
    output wire stm_rx_o,
    output wire stm_rst_o           = 1'bz, // '0' to hold the microcontroller reset line, to free the SD card

    input         SPI_SCK,
    output        SPI_DO,
    input         SPI_DI,
    input         SPI_SS2,
    //output wire   SPI_WAIT        = 1'b1, // '0' to hold the microcontroller data streaming
    //inout [31:0] GPIO,
    output LED                    = 1'b1 // '0' is LED on
);

localparam CONF_STR = {
    "P,CORE_NAME.dat;", 
//    "P,Donkey Kong.dat;",
    "S,DAT,Alternative ROM...;", 
    "O12,Screen Rotate,0,90,180,270;",
    "O34,Scanlines,Off,25%,50%,75%;",
    "O5,Blend,Off,On;",
    "O7,Scandoubler,On,Off;",
    "T0,Reset;"
};


//---------------------------------------------------------
//-- MC2+ defaults
//---------------------------------------------------------
//assign GPIO = 32'bzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz;
assign stm_rst_o    = 1'bZ;
assign stm_rx_o = 1'bZ;
assign LED  = ~ioctl_downl;

//no SRAM for this core
assign sram_we_n_o  = 1'b1;
assign sram_oe_n_o  = 1'b1;

//all the SD reading goes thru the microcontroller for this core
assign sd_cs_n_o = 1'bZ;
assign sd_sclk_o = 1'bZ;
assign sd_mosi_o = 1'bZ;

wire joy1_up_i, joy1_down_i, joy1_left_i, joy1_right_i, joy1_p6_i, joy1_p9_i;
wire joy2_up_i, joy2_down_i, joy2_left_i, joy2_right_i, joy2_p6_i, joy2_p9_i;

//joystick_serial  joystick_serial 
//(
//    .clk_i           ( clock_24 ),
//    .clk_en          ( clk_cnt[1] ),
//    .joy_data_i      ( joy_data_i ),
//    .joy_clk_o       ( joy_clock_o ),
//    .joy_load_o      ( joy_load_o ),
//
//    .joy1_up_o       ( joy1_up_i ),
//    .joy1_down_o     ( joy1_down_i ),
//    .joy1_left_o     ( joy1_left_i ),
//    .joy1_right_o    ( joy1_right_i ),
//    .joy1_fire1_o    ( joy1_p6_i ),
//    .joy1_fire2_o    ( joy1_p9_i ),
//
//    .joy2_up_o       ( joy2_up_i ),
//    .joy2_down_o     ( joy2_down_i ),
//    .joy2_left_o     ( joy2_left_i ),
//    .joy2_right_o    ( joy2_right_i ),
//    .joy2_fire1_o    ( joy2_p6_i ),
//    .joy2_fire2_o    ( joy2_p9_i )
//);

joydecoder joystick_serial  (
    .clk          ( clock_24 ), 	//24000000/16 --> 16 = 2^(3+1) joydecoder div=3
    .joy_data     ( joy_data_i ),
    .joy_clk      ( joy_clock_o ),
    .joy_load     ( joy_load_o ),
	 .clock_locked ( pll_locked ),

    .joy1up       ( joy1_up_i ),
    .joy1down     ( joy1_down_i ),
    .joy1left     ( joy1_left_i ),
    .joy1right    ( joy1_right_i ),
    .joy1fire1    ( joy1_p6_i ),
    .joy1fire2    ( joy1_p9_i ),

    .joy2up       ( joy2_up_i ),
    .joy2down     ( joy2_down_i ),
    .joy2left     ( joy2_left_i ),
    .joy2right    ( joy2_right_i ),
    .joy2fire1    ( joy2_p6_i ),
    .joy2fire2    ( joy2_p9_i )
); 


// ROM Data Pump
reg [7:0] pump_s = 8'b11111111;
PumpSignal PumpSignal (clock_24, ~pll_locked, ioctl_downl, pump_s);

//-----------------------------------------------------------------

wire  [1:0] scanlines = status[4:3];
wire        blend = status[5];

assign      AUDIO_R = AUDIO_L;
//assign      SDRAM_CLK = clock_24;
assign      SDRAM_CKE = 1;

reg [1:0]clk_cnt = 2'd0;
always @(posedge clock_24)
begin
    clk_cnt <= clk_cnt + 2'd1;
end

wire pll_locked,clock_24;
pll pll(
    .locked(pll_locked),
    .inclk0(clock_50_i),
    .c0(clock_24),//W_CLK_24576M
    .c1(SDRAM_CLK)
    );

wire clk_25m2,clk_40;    
pll_vga pll_vga(
    .inclk0(clock_50_i),
    .c0(clk_25m2),
    .c1(clk_40)
    );

wire [15:0] main_rom_a;
wire [15:0] main_rom_do;
wire [11:0] sub_rom_a;
wire [15:0] sub_rom_do;
wire [18:0] wav_rom_a;
wire [15:0] wav_rom_do;

wire        ioctl_downl;
wire  [7:0] ioctl_index;
wire        ioctl_wr;
wire [24:0] ioctl_addr;
wire  [7:0] ioctl_dout;

data_io #(.STRLEN(($size(CONF_STR)>>3)))
data_io(
    .clk_sys       ( clock_24     ),
    .SPI_SCK       ( SPI_SCK      ),
    .SPI_SS2       ( SPI_SS2      ),
    .SPI_DI        ( SPI_DI       ),
    .SPI_DO        ( SPI_DO       ),
    
    .data_in       ( pump_s & osd_s ),
    .conf_str      ( CONF_STR     ),
    .status        ( status       ),
    .core_mod      (core_mod      ),

    .ioctl_download( ioctl_downl  ),
    .ioctl_index   ( ioctl_index  ),
    .ioctl_wr      ( ioctl_wr     ),
    .ioctl_addr    ( ioctl_addr   ),
    .ioctl_dout    ( ioctl_dout   )
);


reg port1_req, port2_req;
sdram #(24) sdram(
    .*,
    .init_n        ( pll_locked   ),
    .clk           ( clock_24     ),

    .port1_req     ( port1_req    ),
    .port1_ack     ( ),
    .port1_a       ( ioctl_addr[23:1] ),
    .port1_ds      ( {ioctl_addr[0], ~ioctl_addr[0]} ),
    .port1_we      ( ioctl_downl ),
    .port1_d       ( {ioctl_dout, ioctl_dout} ),
    .port1_q       ( ),

    .cpu1_addr     ( ioctl_downl ? 16'hffff : {2'b00, main_rom_a[14:1]} ),
    .cpu1_q        ( main_rom_do ),
    .cpu2_addr     ( ioctl_downl ? 16'hffff : sub_rom_a[11:1] + 16'h7000 ),
    .cpu2_q        ( sub_rom_do ),

    // port2 for sprite graphics
    .port2_req     ( port2_req ),
    .port2_ack     ( ),
    .port2_a       ( ioctl_addr[23:1] ),
    .port2_ds      ( {ioctl_addr[0], ~ioctl_addr[0]} ),
    .port2_we      ( ioctl_downl ),
    .port2_d       ( {ioctl_dout, ioctl_dout} ),
    .port2_q       ( ),

    .wav_addr       ( ioctl_downl ? 18'h3ffff : wav_rom_a[18:1]),
    .wav_q          ( )//wav_rom_do )
);

// ROM download controller
always @(posedge clock_24) begin
    reg        ioctl_wr_last = 0;

    ioctl_wr_last <= ioctl_wr;
    if (ioctl_downl) begin
        if (~ioctl_wr_last && ioctl_wr) begin
            port1_req <= ~port1_req;
            port2_req <= ~port2_req;
        end
    end
end

dpram #(17) rom
(
    .clock_a(clock_24),
    .wren_a(ioctl_wr && ioctl_addr >= 17'h10000),
    .address_a(ioctl_addr[16:0]),
    .data_a(ioctl_dout),

    .clock_b(clock_24),
    .address_b(wav_rom_a[16:0]),
    .q_b(wav_rom_do[7:0])
);

reg reset = 1;
reg rom_loaded = 0;
always @(posedge clock_24) begin
    reg ioctl_downlD;
    ioctl_downlD <= ioctl_downl;
    if (ioctl_downlD & ~ioctl_downl) rom_loaded <= 1;
    //reset <= status[0] | ~btn_n_i[4] | ~rom_loaded;
	 reset <= status[0] | ~rom_loaded;
end

wire  [7:0] audio;
wire        hs_n, vs_n;
wire        vb;
wire        blankn = ~vb;//~(hb | vb);
wire [3:0]  r, g, b;

dkong_top dkong(                   
    .I_CLK_24576M(clock_24),
    .O_PIX(clk_pix),
    .I_RESETn(~reset),
    .I_U1(~m_up),
    .I_D1(~m_down),
    .I_L1(~m_left),
    .I_R1(~m_right),
    .I_J1(~m_fireA),
    .I_U2(~m_up2),
    .I_D2(~m_down2),
    .I_L2(~m_left2),
    .I_R2(~m_right2),
    .I_J2(~m_fire2A),
    .I_S1(~m_one_player),
    .I_S2(~m_two_players),
    .I_C1(~m_coin1),
    .I_DIP_SW(status[15:8]),
    .I_DKJR(core_mod[0]),
    .I_DK3B(core_mod[1]),
    .I_RADARSCP(core_mod[2]),
    .I_PESTPLCE(core_mod[3]),
    .O_SOUND_DAT(audio),
    .O_VGA_R(r),
    .O_VGA_G(g),
    .O_VGA_B(b),
    .O_H_BLANK(hbl0),
    .O_V_BLANK(vb),
    .O_VGA_H_SYNCn(hs_n),
    .O_VGA_V_SYNCn(vs_n),

    .DL_ADDR(ioctl_addr[15:0]),
    .DL_WR(ioctl_wr && ioctl_addr[23:16] == 0),
    .DL_DATA(ioctl_dout),
    .MAIN_CPU_A(main_rom_a),
    .MAIN_CPU_DO(main_rom_a[0] ? main_rom_do[15:8] : main_rom_do[7:0]),
    .SND_ROM_A(sub_rom_a),
    .SND_ROM_DO(sub_rom_a[0] ? sub_rom_do[15:8] : sub_rom_do[7:0]),
    .WAV_ROM_A(wav_rom_a),
    .WAV_ROM_DO(/*wav_rom_a[0] ? wav_rom_do[15:8] :*/ wav_rom_do[7:0])
    );

wire clk_pix, hbl0;
wire hb = hbl[7];
reg [7:0] hbl;
always @(posedge clk_pix) 
begin
        hbl <= (hbl<<1)|hbl0;
end

wire [11:0] vga_col_s;
wire vga_hs_s, vga_vs_s;

framebuffer #(256,224,12) framebuffer
(
        .clk_sys    ( clock_24 ),
        .clk_i      ( clk_pix ),
        .RGB_i      ((blankn) ? {r,g,b} : 12'b000000000000 ),//idx_color_s ),
        .hblank_i   ( hb ),
        .vblank_i   ( vb ),
        
        .rotate_i   ( status[2:1] ), 

        .clk_vga_i  ( (status[1]) ? clk_40 : clk_25m2 ), //800x600 or 640x480
        .RGB_o      ( vga_col_s ),
        .hsync_o    ( vga_hs_s ),
        .vsync_o    ( vga_vs_s ),
        .blank_o    (  ),

        .odd_line_o (  )
);

wire direct_video_s = ~status[7] ^ direct_video;

mist_video #(.COLOR_DEPTH(4),.SD_HCNT_WIDTH(10), .USE_FRAMEBUFFER(1)) mist_video(
    .clk_sys( direct_video_s ? clock_24 :(status[1]) ? clk_40 : clk_25m2),
    .SPI_SCK(SPI_SCK),
    .SPI_SS3(SPI_SS2),
    .SPI_DI(SPI_DI),

    .R       ( (direct_video_s) ? (blankn) ? r : 4'b0000 : vga_col_s[11:8] ),
    .G       ( (direct_video_s) ? (blankn) ? g : 4'b0000 : vga_col_s[7:4]  ),
    .B       ( (direct_video_s) ? (blankn) ? b : 4'b0000 : vga_col_s[3:0]  ),
    .HSync   ( (direct_video_s) ? hs_n : vga_hs_s ),
    .VSync   ( (direct_video_s) ? vs_n : vga_vs_s ),
    
    .VGA_R(VGA_R),
    .VGA_G(VGA_G),
    .VGA_B(VGA_B),
    .VGA_VS(VGA_VS),
    .VGA_HS(VGA_HS),

    .ce_divider ( 1'b1 ),
    .blend      ( blend ),
    .scandoubler_disable(direct_video_s),
    .scanlines  ( scanlines ),
    .no_csync   ( ~direct_video_s ),
    .rotate     ( osd_rotate ),
    .osd_enable ( osd_enable )
    );

wire [31:0] status;
wire  [1:0] buttons;
wire  [1:0] switches;
wire  [7:0] joystick_0;
wire  [7:0] joystick_1;
wire        scandoublerD;
wire        ypbpr;
wire        no_csync;
wire  [6:0] core_mod;
wire        key_strobe;
wire        key_pressed;
wire  [7:0] key_code;

/*
user_io #(
    .STRLEN(($size(CONF_STR)>>3)))
user_io(
    .clk_sys        (clock_24       ),
    .conf_str       (CONF_STR       ),
    .SPI_CLK        (SPI_SCK        ),
    .SPI_SS_IO      (CONF_DATA0     ),
    .SPI_MISO       (SPI_DO         ),
    .SPI_MOSI       (SPI_DI         ),
    .buttons        (buttons        ),
    .switches       (switches       ),
    .scandoubler_disable (scandoublerD),
    .ypbpr          (ypbpr          ),
    .no_csync       (no_csync       ),
    .core_mod       (core_mod       ),
    .key_strobe     (key_strobe     ),
    .key_pressed    (key_pressed    ),
    .key_code       (key_code       ),
    .joystick_0     (joystick_0     ),
    .joystick_1     (joystick_1     ),
    .status         (status         )
    );
*/
dac #(
    .C_bits(8))
dac(
    .clk_i(clock_24),
    .res_n_i(1'b1),
    .dac_i(audio),
    .dac_o(AUDIO_L)
    );

//i2s audio
wire MCLK;

audio_top i2s
(
	.clk_50MHz(clock_50_i),
	.dac_MCLK (MCLK),
	.dac_LRCK (LRCLK),
	.dac_SCLK (SCLK),
	.dac_SDIN (SDIN),
	.L_data   ({1'b0, audio, audio [7:1]}),
	.R_data   ({1'b0, audio, audio [7:1]})
);
	 	 
//------------------------------------------------------------
    
wire m_up, m_down, m_left, m_right, m_fireA, m_fireB, m_fireC, m_fireD, m_fireE, m_fireF, m_fireG;
wire m_up2, m_down2, m_left2, m_right2, m_fire2A, m_fire2B, m_fire2C, m_fire2D, m_fire2E, m_fire2F, m_fire2G;
wire m_tilt, m_coin1, m_coin2, m_coin3, m_coin4, m_one_player, m_two_players, m_three_players, m_four_players;

wire m_right4, m_left4, m_down4, m_up4, m_right3, m_left3, m_down3, m_up3;

//wire btn_one_player  = ~btn_n_i[1] | m_one_player;
//wire btn_two_players = ~btn_n_i[2] | m_two_players;
//wire btn_coin        = ~btn_n_i[3] | m_coin1;

wire btn_one_player  = m_one_player;
wire btn_two_players = m_two_players;
wire btn_coin        = m_coin1;

wire kbd_intr;
wire [7:0] kbd_scancode;
wire [7:0] osd_s;

//get scancode from keyboard
io_ps2_keyboard keyboard 
 (
  .clk       ( clock_24 ),
  .kbd_clk   ( ps2_clk_io ),
  .kbd_dat   ( ps2_data_io ),
  .interrupt ( kbd_intr ),
  .scancode  ( kbd_scancode )
);

wire [15:0]joy1_s;
wire [15:0]joy2_s;
wire [8:0]controls_s;
wire osd_enable;
wire direct_video;
wire [1:0]osd_rotate;

//translate scancode to joystick
//kbd_joystick #( .OSD_CMD    ( 3'b011 ), .USE_VKP( 1'b1), .CLK_SPEED(24500)) k_joystick
//(
//    .clk          ( clock_24 ), //24.5mhz
//    .kbdint       ( kbd_intr ),
//    .kbdscancode  ( kbd_scancode ), 
//
//    .joystick_0   ({ joy1_p9_i, joy1_p6_i, joy1_up_i, joy1_down_i, joy1_left_i, joy1_right_i }),
//    .joystick_1   ({ joy2_p9_i, joy2_p6_i, joy2_up_i, joy2_down_i, joy2_left_i, joy2_right_i }),
//      
//    //-- joystick_0 and joystick_1 should be swapped
//    .joyswap      ( 0 ),
//
//    //-- player1 and player2 should get both joystick_0 and joystick_1
//    .oneplayer    ( 1 ),
//
//    //-- tilt, coin4-1, start4-1
//    .controls     ( {m_tilt, m_coin4, m_coin3, m_coin2, m_coin1, m_four_players, m_three_players, m_two_players, m_one_player} ),
//
//    //-- fire12-1, up, down, left, right
//
//    .player1      ( {m_fireG,  m_fireF, m_fireE, m_fireD, m_fireC, m_fireB, m_fireA, m_up, m_down, m_left, m_right} ),
//    .player2      ( {m_fire2G, m_fire2F, m_fire2E, m_fire2D, m_fire2C, m_fire2B, m_fire2A, m_up2, m_down2, m_left2, m_right2} ),
//
//    .direct_video ( direct_video ),
//    .osd_rotate   ( osd_rotate ),
//
//    //-- keys to the OSD
//    .osd_o        ( osd_s ),
//    .osd_enable   ( osd_enable ),
//
//    //-- sega joystick
//    .sega_strobe  ( joy_p7_o )
//
//        
//);
kbd_joystick_ua #( .OSD_CMD ( 3'b011 ), .USE_VKP( 1'b1)) k_joystick
(
    .clk          ( clock_24 ), //24.5mhz
    .kbdint       ( kbd_intr ),
    .kbdscancode  ( kbd_scancode ), 

    .joystick_0   ({ joy1_p9_i, joy1_p6_i, joy1_up_i, joy1_down_i, joy1_left_i, joy1_right_i }),
    .joystick_1   ({ joy2_p9_i, joy2_p6_i, joy2_up_i, joy2_down_i, joy2_left_i, joy2_right_i }),
      
    //-- joystick_0 and joystick_1 should be swapped
    .joyswap      ( 0 ),

    //-- player1 and player2 should get both joystick_0 and joystick_1
    .oneplayer    ( core_mod[3]? 0 : 1 ),

    //-- tilt, coin4-1, start4-1
    .controls     ( {m_tilt, m_coin4, m_coin3, m_coin2, m_coin1, m_four_players, m_three_players, m_two_players, m_one_player} ),

    //-- fire12-1, up, down, left, right

    .player1      ( {m_fireG,  m_fireF, m_fireE, m_fireD, m_fireC, m_fireB, m_fireA, m_up, m_down, m_left, m_right} ),
    .player2      ( {m_fire2G, m_fire2F, m_fire2E, m_fire2D, m_fire2C, m_fire2B, m_fire2A, m_up2, m_down2, m_left2, m_right2} ),

    .direct_video ( direct_video ),
    .osd_rotate   ( osd_rotate ),

    //-- keys to the OSD
    .osd_o        ( osd_s ),
    .osd_enable   ( osd_enable ),

    //-- sega joystick
    .sega_clk     ( hs_n ),		
    .sega_strobe  ( joy_p7_o )
);

endmodule
