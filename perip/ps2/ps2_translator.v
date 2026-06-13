module ps2_translator(
    input         clock,
    input  [7:0]  keycode,
    input         keycode_valid,
    input         keycode_extend,
    input         keycode_release,
    output [7:0]  amcode
);
    import "DPI-C" function void key_handler(input byte keycode, output byte data, input byte is_extend, input byte is_release);
    reg [7:0] amcode_reg;
    assign amcode = amcode_reg;
    always @(posedge clock) begin
        if (keycode_valid) key_handler(keycode, amcode_reg, {7'b0,keycode_extend}, {7'b0,keycode_release});
    end
endmodule
