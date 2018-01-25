This is an attempt at reverse engineering the circuit format
in the game Robot Odyssey

The cricuit format .csv is as follows:

000: 8x output-state
008: [ gate FF ] wires
nnn: {padding (value does not matter)}
400: 8x in_or_out
408: nnn (size of initial block) 00
40A: labelx18 00
41D: "1 " labelx32 00
440: "2 " labelx32 00
463: ...
510: "8 " labelx32 00

in_or_out: 00 = neither, 01 = in, 02 = out

wires: 07 [ 00 input [ 00 out ] FF ]

And-gate: 01 i1s i2s [ 00 out ]
Or-gate:  02 i1s i2s [ 00 out ]
Xor-gate: 03 i1s i2s [ 00 out ]
Not-gate: 04 i1s [ 00 out ]

out: Either a number 0-7 signifying the outputs of the circuit
     or an index for the position of an input for a gate in the file.

Example: (And-gate)
000: 00 00 00 00 00 00 00 00 -- No inputs are powered
008: 01 00 00 00 01 FF       -- An and-gate connecting to output 01
00E: 07                      -- Begin wires
00F: 00 02 00 09 FF          -- A wire connecting input 02 to the
				first input (09) of the and-gate.
014: 00 03 00 0A FF          -- A wire connecting input 03 to the
                                second input (0A) of the and-gate.
01A-3FF: 00                  -- Padding
400: 00 02 01 01 00 00 00 00 -- Connection 01 is output and 02,03 are inputs.
408: 1A 00                   -- Length of the initial block
40A-534: {text}              -- Description of the circuit
