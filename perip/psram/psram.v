`define USE_QPI

module psram(
  input sck,
  input ce_n,
  inout [3:0] dio
);
  parameter idle = 6'b000001,
            cmd  = 6'b000010,
            addr = 6'b000100,
            rwait= 6'b001000,
            rdata= 6'b010000,
            wdata= 6'b100000;
  parameter cmd_QIOR = 8'hEB, // Quad I/O Read command, can be adjusted based on the specific PSRAM used
            cmd_QIOW = 8'h38; // Quad I/O Write command, can be adjusted based on the specific PSRAM used

  import "DPI-C" function void psram_read(input int addr, output int data);
  import "DPI-C" function void psram_write(input int addr, input byte data, input byte offset);

  reg [5:0] state;
  reg [2:0] bit_cnt;
  reg [7:0] cmd_reg;
  reg [23:0] addr_reg;
  reg [31:0] rdata_reg;
  reg [3:0] wdata_reg;
  
  always @(posedge sck or posedge ce_n) begin
    if (ce_n) begin
      state <= idle;
      bit_cnt <= 0;
    end else begin
      case (state)
      
`ifdef USE_QPI      
        idle: begin
          if (!ce_n) begin
            state <= cmd;
            cmd_reg[7:4] <= dio; // start bit
            bit_cnt <= 3'b1;
          end
        end
        cmd: begin
            state <= addr;
            cmd_reg[3:0] <= dio;
            bit_cnt <= 0;
        end
`else
        idle: begin
          if (!ce_n) begin
            state <= cmd;
            cmd_reg <= {7'b0, dio[0]}; // start bit
            bit_cnt <= 3'b1;
          end
        end
        cmd: begin
          if (bit_cnt != 7) begin
            cmd_reg <= {cmd_reg[6:0], dio[0]};
            bit_cnt <= bit_cnt + 1;
          end else begin
            state <= addr;
            cmd_reg <= {cmd_reg[6:0], dio[0]};
            bit_cnt <= 0;
          end
        end
`endif

        addr: begin
          if (bit_cnt != 5) begin
            addr_reg <= {addr_reg[19:0], dio};
            bit_cnt <= bit_cnt + 1;
          end else begin
            if (cmd_reg != cmd_QIOR && cmd_reg != cmd_QIOW) begin
              $fwrite(32'h80000002, "Assertion failed: Unsupport command `%xh`, only support `EBh` read command and `38h` write command\n", cmd_reg);
              $fatal;
            end
            state <= cmd_reg == cmd_QIOR ? rwait : wdata;
            addr_reg <= {addr_reg[19:0], dio};
            bit_cnt <= 0;
          end
        end
        rwait: begin
          // wait state before reading data, can be adjusted based on timing requirements 
          if (bit_cnt != 6) begin
            bit_cnt <= bit_cnt + 1;
          end else begin
            state <= rdata;
            psram_read({8'b0,addr_reg}, rdata_reg);
            bit_cnt <= 0;
          end
        end
        rdata: begin
          if (bit_cnt != 7) begin
            bit_cnt <= bit_cnt + 1;
            if (bit_cnt[0] == 1'b1) begin
              rdata_reg <= { 8'b0,rdata_reg[31:8]}; 
            end
          end else begin
            state <= idle;
          end
        end
        wdata: begin
          if (bit_cnt != 7) begin
            if (bit_cnt[0] == 1'b0) begin
              wdata_reg <= dio;
            end else begin
              psram_write({8'b0,addr_reg},{wdata_reg,dio},{6'b0,bit_cnt[2:1]});
            end
            bit_cnt <= bit_cnt + 1;
          end else begin
            psram_write({8'b0,addr_reg},{wdata_reg,dio},{6'b0,bit_cnt[2:1]});
            state <= idle;
          end
        end
        default: state <= idle;
      endcase
    end
  end

  wire [7:0] out_byte;
  wire [3:0] out_half_byte;
  assign out_byte = state != rdata ? 8'bz : rdata_reg[7:0]; // output data during read state
  assign out_half_byte = bit_cnt[0] == 1'b0 ? out_byte[7:4] : out_byte[3:0];
  assign dio = out_half_byte;

endmodule
