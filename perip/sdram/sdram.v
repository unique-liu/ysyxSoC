module sdram(
  input        clk,
  input        cke,
  input        cs,
  input        ras,
  input        cas,
  input        we,
  input [12:0] a,
  input [ 1:0] ba,
  input [ 1:0] dqm,
  inout [15:0] dq
);
  parameter CMD_ACTIVE    = 5'b10011,//[cke,cs,ras,cas,we] = 10011
            CMD_READ      = 5'b10101,//[cke,cs,ras,cas,we] = 10101
            CMD_WRITE     = 5'b10100,//[cke,cs,ras,cas,we] = 10100
            CMD_LOAD_MODE = 5'b10000;//[cke,cs,ras,cas,we] = 10000
  parameter idle          = 4'b0001,
            active        = 4'b0010,
            read          = 4'b0100,
            write         = 4'b1000;
  reg [3:0] state;


  import "DPI-C" function void sdram_read(input int addr, output shortint data, input byte burst_times);
  import "DPI-C" function void sdram_write(input int addr, input shortint data, input byte burst_times,input byte dqm);
  wire [4:0] cmd = {cke, cs, ras, cas, we};
  reg [2:0] mode_CAS_latency;//6:4   001:1 010:2 011:3
  reg [2:0] mode_burst_length;//2:0  000:1 001:2 010:4 011:8 111:full page

  wire [2:0] real_burst_length;
  reg [2:0] latency_counter;
  reg [2:0] burst_counter;
  reg [15:0] data_buffer;
  reg [12:0] row_addr[3:0];
  reg [8:0] col_addr;
  reg [1:0] ba_addr;

  assign real_burst_length = (mode_burst_length == 3'b000) ? 3'b1 :
                              (mode_burst_length == 3'b001) ? 3'b10 :
                              (mode_burst_length == 3'b010) ? 3'b100 :
                              (mode_burst_length == 3'b011) ? 3'b111 :
                              3'b111;

  always @(posedge clk) begin
      case (state)
        idle: begin
          if (cmd == CMD_ACTIVE) begin
            state <= active;
            row_addr[ba] <= a[12:0];
          end else if (cmd == CMD_LOAD_MODE) begin
            mode_CAS_latency <= a[6:4];
            mode_burst_length <= a[2:0];
          end
        end
        active: begin
          if (cmd == CMD_READ) begin
            state <= read;
            sdram_read({7'b0,row_addr[ba],ba,a[8:0],1'b0}, data_buffer, 8'b0);
            latency_counter <= 3'b1;
            burst_counter <= 3'b1;
            col_addr <= a[8:0];
            ba_addr <= ba;
          end else if (cmd == CMD_ACTIVE) begin
            row_addr[ba] <= a[12:0];
          end else if (cmd == CMD_LOAD_MODE) begin
            mode_CAS_latency <= a[6:4];
            mode_burst_length <= a[2:0];
          end else if (cmd == CMD_WRITE) begin
            state <= write;
            sdram_write({7'b0,row_addr[ba],ba,a[8:0],1'b0}, dq, 8'b0, {6'b0,dqm});
            burst_counter <= 3'b1;
            col_addr <= a[8:0];
            ba_addr <= ba;
          end
        end
        read: begin
          if (latency_counter < mode_CAS_latency) begin
            latency_counter <= latency_counter + 1;
          end else if (burst_counter < real_burst_length) begin
            sdram_read({7'b0,row_addr[ba_addr],ba_addr,col_addr,1'b0}, data_buffer, {5'b0,burst_counter});
            burst_counter <= burst_counter + 1;
          end else begin
            state <= active;
          end
        end
        write: begin
          if (burst_counter < real_burst_length) begin
            sdram_write({7'b0,row_addr[ba_addr],ba_addr,col_addr,1'b0}, dq, {5'b0,burst_counter}, {6'b0,dqm});
            burst_counter <= burst_counter + 1;
          end else begin
            state <= active;
          end
        end
        default: state <= idle;
      endcase
  end

  assign dq = (latency_counter==mode_CAS_latency && state == read) ? data_buffer : 16'bz;

endmodule
