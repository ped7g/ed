Z80ASM = sjasmplus
Z80ASMFLAGS = --msg=war --lst=ed.lst --lstlab --nologo --dirbol --fullpath --syntax=abfw
OUTPUT_OPT = -D__IGNORE=

# Z80ASM = bin/snasm
# Z80ASMFLAGS = -map
# OUTPUT_OPT =

# Z80ASM = wine ~/wine32/zx/CSpect/CSpect_current/snasm.exe
# Z80ASMFLAGS = -map
# OUTPUT_OPT =

OUTPUT := ed.sna

INPUTFILES := $(wildcard src/*.s) $(wildcard data/*.*) data/tilemap_font_8x6.i.s Makefile
MAINSOURCE := src/ed.s
EXTRA_OUTPUTS := ed.sna.map ed.lst

.PHONY: build clean

build : $(OUTPUT)

clean :
	$(RM) -f $(OUTPUT) $(EXTRA_OUTPUTS)

$(OUTPUT) : $(INPUTFILES) $(MAINSOURCE)
	$(Z80ASM) $(Z80ASMFLAGS) $(MAINSOURCE) $(OUTPUT_OPT)$(OUTPUT)

# tilemap font 8x6 conversion from GIMP raw data export (each pixel separate byte, must be 0-15 index only)
# -> to asm source for including by sjasmplus, having two 4b pixels together

data/tilemap_font_8x6.i.s : data/font_8x6_source/font_8x6.tga
	hexdump -s 66 -v -e '"\tHEX\t" 8/1 "%01X" " " 8/1 "%01X" " " 8/1 "%01X" "\n\tHEX\t" 8/1 "%01X" " " 8/1 "%01X" " " 8/1 "%01X" "\n\tHEX\t" 8/1 "%01X" " " 8/1 "%01X" "\n"' $^ > $@

data/font_8x6_source/font_8x6.tga : data/font_8x6_source/font_8x6.png
	convert $^ -flip tga:$@
