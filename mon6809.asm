	cpu	6809

;	Created at						20 jan  2021.
;	Scrumpel 8a, 8b and 8d monitor
;	device addresses.

aciadata		equ	$cf01		;ACIA Data register
aciastat		equ	$cf00		;ACIA Status register
leds			equ	$cf20		;LEDS
aciainit		equ	%00010101	;8N1 @ 115200

;	pointers and storage.

more_ctr		equ	12
more_max		equ	13

mbyte			equ	$10
keycommand		equ	$11
keycharacter		equ	$12
counter			equ	$13
buffer			equ	$14
tobuf			equ	$16
crc			equ	$18
teller			equ	$19
movscr			equ	$1A

breakpoint		equ	$20
reg_base		equ	$22
rega			equ	reg_base
regb			equ	rega+1
regpsw			equ	regb+1
regx			equ	regpsw+1
regy			equ	regx+2
regdp			equ	regy+2
regup			equ	regdp+1
regsp			equ	regup+2
regpc			equ	regsp+2
tempstor		equ	regpc+2

consolebreak	    	equ	tempstor+2
old103d			equ	consolebreak+1
intvar			equ	old103d+1

;	special characters.

cr			equ	$d
lf			equ	$a
esc			equ	$1b
cls			equ	$0c
xon			equ	$11
xoff			equ	$13
space			equ	$20
bs			equ	8
ctlg			equ	7

irq_vector		equ	$0f3
xirq_vector		equ	$0f0
realtime_interrupt	equ	$0fc
ramswi			equ	$0a1
resetflag		equ	$06e


stackbase		equ	$bfff

	org	0
	phase	$c000

	jmp	start

jump_table

	jmp	in			;Serial input.
	jmp	inul			;Direct serial input.
	jmp	bytot			;Serial byte out.
	jmp	getnibble		;Serial nibble input.
	jmp	bytin			;Serial byte in.
	jmp	ot			;Serial output.
	jmp	ott			;Serial text out.
	jmp	xot			;Serial X out.
	jmp	return			;Return monitor.
	jmp	bitot			;Bitot <a>.
	jmp	adresin			;Serial HEX adres in in X.
	jmp	crlf			;Serial carriage linefeed.
	jmp	mesgot			;Display message #(A) vanaf (X).
	pshs	d			;Delay (x) maal 3 microsecondes @ 8 MHz.
	bra	delop1

delay2	pshs	d			;	  cycles
	ldd	#4000			;	3 cycles
delop1	subd	#1			;	4 cycles
	bne	delop1			;	6 cycles Branch taken
					;	5 cycles Branch not taken
	puls	d			;	  cycles
	rts				;	5 cycles

ot	pshs	b			;Save B register.
	ldb	more_ctr		;Get more counter value.
	beq	no_more			;If zero, more is switched off.
	cmpa	#lf			;Count lf.
	bne	no_more
	dec	more_ctr		;Count more until zero.
	bne	no_more
	pshs	x			;Save X.
	ldx	#more_txt		;Point X to moretext.
	jsr	ott			;Print it.
	jsr	in			;Wait for a key pressed.
	lda	#cr			;CR in A.
	puls	x			;Get X back.
no_more ldb	aciastat		;Read ACIA status.
	bitb	#$1			;Key pressed?
	beq	otlp			;No? Send character.
	jsr	checkxoff		;Check if XOFF.
otlp	ldb	aciastat		;Read ACIA status.
	bitb	#$2
	beq	otlp			;Transmitter not empty then wait.
	sta	aciadata		;Transmit the character.
	puls	b			;Restore B.
	rts

checkxoff

	ldb	aciastat
	bitb	#$1
	beq	exckff
	ldb	aciadata		;Get data.
	andb	#$7f
	cmpb	#xoff			;Check for XOFF.
	bne	chkbrk

	pshs	a			;Save A.

wait_for_xon

	jsr	in			;Get a character.
	cmpa	#xon			;Is it XON?
	bne	wait_for_xon		;NO? then wait.
	puls	a			;Restore A.

exckff	rts

chkbrk	cmpb	#esc			;Is ESC pressed?
	bne	exckff			;No then continue.
	ldb	#0			;Set breakflag on BREAK.
	stb	consolebreak
	bra	exckff			;Continue.

more_txt

	fcb	lf,"--- more ---",0

ott	lda	0,x+			;Display text pointed by X and inc X.
	beq	exits			;Character=0? Then stop.
	jsr	ot			;Print the character.
	bra	ott			;Get next character.
exits	rts

bytot	pshs	a			;Save A.
	pshs	b			;Save B.
	sta	mbyte			;Save hex-byte.
	ldb	#2			;Print two nibbles.
	lsra
	lsra
	lsra
	lsra
bytlp	anda	#$0f			;Mask lower byte.
	pshs	a			;Save A.
	tfr	cc,a
	anda	#%11011100
	tfr	a,cc
	puls	a			;Restore A.
	daa				;Decimal adjust.
	adda	#$f0
	adca	#$40
	jsr	ot			;Print the HEX digit.
	lda	mbyte			;Restore hex-byte.
	decb
	bne	bytlp			;Get next nibble.
	puls	b			;Restore B.
	puls	a			;Restore A.
	rts

escape	ldx	#tesc			;Point X to esc text.
	jsr	ott			;Print it.
	jmp	return			;Return to monitor.

inul	pshs	x			;Save X.
	pshs	b			;Save B.
	ldb	more_max
	stb	more_ctr
inlp	ldb	aciastat		;Get ACIA status.
	bitb	#$1
	beq	inlp			;Wait until character sent.
	lda	aciadata		;Get character.
	anda	#$7f			;Remove eight bit.
	cmpa	#esc			;Check for esc key.
	beq	escape			;Pressed? Goto escape.
	puls	b			;Restore B.
	puls	x			;Restore X.
	rts

in	bsr	inul			;Get character.
	cmpa	#'z'
	beq	conup
	bcs	nolc
	rts
conup	anda	#$5f			;Convert to uppercase.
	rts
nolc	cmpa	#'a'
	beq	conup
	bcc	conup
	rts

getnibble

	jsr	in			;Get character.
alnibin	cmpa	#'0'			;Is it a 0?
	bcs	error			;No, then error.
	cmpa	#$3a
	bcs	sub0
	cmpa	#'A'
	bcs	error
	cmpa	#'G'
	bcc	error
	jsr	ot			;Echo character.
	suba	#7
	bra	add0
sub0	jsr	ot			;Echo character.
add0	suba	#'0'
	andcc	#$FE			;Clear carry.
	rts
error	orcc	#1			;Set carry.
	rts

bytin	jsr	getnibble		;Get high nibble.
	bcs	bytin			;No HEX number? Retry.
albytin	sta	mbyte			;Store it in mbyte.
bytin1	jsr	getnibble		;Get low nibble.
	bcs	bytin1			;No HEX number? Retry.
	tfr	a,b			;Move A to B.
	lda	mbyte			;Get high nibble.
	lsla				;Shift to left * 16.
	lsla
	lsla
	lsla
	pshs	b			;Add B to A to get a complete byte.
	adda	,s+
	rts

adresin	jsr	bytin			;Get high byte of address.
	pshs	a			;Save A.
	jsr	bytin			;Get low byte of address.
	tfr	a,b			;Save A into B.
	puls	a			;Restore A.
	exg	d,x			;Transfer D to X.
	rts

xot	pshs	x			;Save X.
	exg	d,x			;Transfer X to D.
	pshs	b			;Save B.
	jsr	bytot			;Print higher byte.
	puls	a			;Restore in A.
	jsr	bytot			;Print lower byte.
	puls	x			;Restore X.
	rts

ecls	ldx	#clscode		;Point X to cls string.
	jmp	ott			;Print it.

clscode	fcb	cls,0			;Clearscreen sequence for your terminal.

crlf	lda	#cr			;Display CR/LF.
	jsr	ot
	lda	#lf
	jmp	ot

softreset

	ldx	$FFFe
	jmp	0,x

montxt	fcb	"6809 Monitor version 1.1/SER working at the Scrumpel 8 SBC",cr,lf
	fcb	"development board, (c) 2022 by N. Brok the Netherlands.",cr,lf,0
prompt	fcb	cr,lf,"6809> ",0
tesc	fcb	cr,lf,"Escaped.",0
alt2	fcb	" : ",0

initr	clr	resetflag		;Clear resetflag form BASIC.
	clr	more_max		;Clear MORE.
	clr	more_ctr
	lda	#%00000011		;Reset ACIA.
	sta	aciastat
	clr	leds			;Clear the LEDS.
	lda	#$ff
	sta	consolebreak		;No console break.
	lda	#aciainit
	sta	aciastat		;ACIA at 8N1 115200 baud.

;** initialization from the system variables **

	ldx	#$0
	stx	breakpoint		;Clear breakpoint.
	lda	#$7e			;Initialize SWI vector.
	sta	ramswi
	ldx	#software_interrupt
	stx	ramswi+1
	bsr	clear_reg		;Clear the registers.
	rts

clear_reg

	ldx	#reg_base		;Clear the CPU-registers.
	ldb	#$E
clrlp1	clr	0,x+
	decb
	bne	clrlp1			;All registers done?
	rts

start	lds	#stackbase		;Initialize stackpointer.
	jsr	initr			;Initialize the monitor.
	jsr	ecls			;Clear screen.
	ldx	#montxt			;Display welcome text.
	jsr	ott
reentry	ldx	#prompt			;Display prompt.
	jsr	ott

;Command interpreter.

cmdlp	jsr	in			;Get the command.
	cmpa	#cr
	beq	return
	cmpa	#space			;Ignore space and below.
	bcs	cmdlp
	jsr	ot
	cmpa	#'?'			;? is short for help.
	bne	cmdnxt
	jsr	help
	jmp	return
cmdnxt	pshs	a
	jsr	in
	jsr	ot
	tfr	a,b
	lda	#space
	jsr	ot
	puls	a
	std	intvar
	ldx	#command_table
nextcmd	lda	0,x			;Get command.
	ldb	1,x
	cmpd	#0
	beq	cerror			;Zero? then print errormessage.
	cmpd	intvar
	beq	ishem			;Found? Execute the command.
	leax	4,x			;No? The try next command.
					;Add 4 to X to get next command.
	bra	nextcmd			;Compare the next command.
ishem	leax	2,x			;Add two to X to get the address of the command.
	lda	0,x			;Get Hbyte from address.
	sta	buffer			;Save it into a 16 bits buffer.
	lda	1,x			;Get Lbyte from address.
	sta	buffer+1		;Save this into buffer.
	ldx	buffer			;Load X with address.
	jsr	0,x			;Jump to it as a subroutine.
	jmp	return			;Return to the monitor when returned.

cerror	ldx	#command_error_text	;Point X to errortext.
merror	jsr	ott			;Display errortext.
	
return	lds	#stackbase		;Initialize the stackpointer.
	bra	reentry			;Back to prompt.

more_cmd

	jsr	bytin			;Get number of lines.
	sta	more_max		;Insert a pause after this.
	sta	more_ctr
	jmp	crlf

software_interrupt

;	Execute breakpoint.

	ldx	#break_ex_txt
	jsr	ott			;Display "breakpoint executed".
	puls	a
	sta	regpsw			;Get the registers from stack.
	puls	a
	sta	rega
	puls	a
	sta	regb
	puls	a
	sta	regdp
	puls	x
	stx	regx
	puls	x
	stx	regy
	puls	x
	stx	regup
	puls	x
	leax	-1,x			;PC points to the SWI instruction.
	stx	regpc
	tfr	s,x
	lda	,-x			;Correction of the stackpointer.
	stx	regsp
	jsr	disreg	
	ldx	breakpoint		;Restore byte at breakpoint.
	lda	tempstor
	sta	0,x			;Restore orginal byte.
	ldx	#$0
	stx	breakpoint		;Clear breakpoint.
	bra	return			;Reinitialize Stackpointer.

break_ex_txt

	fcb	"Breakpoint executed.",cr,lf,0

showreg

disreg	jsr	crlf			;Print empty line.
	ldx	#regtxt			;Display the registers.
	jsr	ott
	lda	rega
	jsr	bytot
	lda	#space
	jsr	ot
	lda	regb
	jsr	bytot
	lda	#space
	jsr	ot
	jsr	ot
	lda	regpsw
	jsr	bitot
	lda	#space
	jsr	ot
	jsr	ot
	ldx	regx
	jsr	xot
	lda	#space
	jsr	ot
	ldx	regy
	jsr	xot
	lda	#space
	jsr	ot
	ldx	regsp
	jsr	xot
	lda	#space
	jsr	ot
	ldx	regup
	jsr	xot
	lda	#space
	jsr	ot
	lda	regdp
	jsr	bytot
	lda	#space
	jsr	ot
	jsr	ot
	ldx	regpc
	jsr	xot
	jmp	crlf

regtxt	fcb	" a  b   E F H I N Z V C   ix   iy   sp   up  dp   pc",cr,lf,0

bitot	ldb	#8			;We have 8 bits to process.
bitnx	asla				;One bit to the left, via carry.
	pshs	a			;Save A.
	bcs	bit1			;If carry is set, display an '1'.
bit0	lda	#space
	jsr	ot
	lda	#'0'			;Otherwise display a '0'.
	bra	bitn
bit1	lda	#space
	jsr	ot
	lda	#'1'			;Display a '1'
bitn	jsr	ot
	puls	a
	decb				;Next bit.
	bne	bitnx
	rts

setbreakpoint

	jsr	adresin			;Get breakpointaddress.
	stx	breakpoint		;Save it in breakpoint.
	lda	0,x			;Get the original byte.
	sta	tempstor		;Save it.
	lda	#$3f			;Place SWI instruction
	sta	0,x			;on this location.
	rts
	
command_error_text

	fcb	" Unknown command type ? or HE for help.",0

command_table

	fcb	"AM"
	fdb	alter			;Alter memory command.
	fcb	"BC"
	fdb	copy			;Blockcopy command.
	fcb	"BD"
	fdb	delete			;Blockdelete command.
	fcb	"BI"
	fdb	insert			;Blockinsert command.
	fcb	"CS"
	fdb	ecls			;Clear screen command.
	fcb	"FI"
	fdb	snuffel			;Search command.
	fcb	"GO"
	fdb	goto			;Goto command.
	fcb	"HD"
	fdb	hdump			;Hexdump command.
	fcb	"AR"
	fdb	modifyreg		;Modify registers command.
	fcb	"MO"
	fdb	more_cmd		;More command.
	fcb	"PM"
	fdb	preset			;Preset command.
	fcb	"SR"
	fdb	showreg			;Showregisters command.
	fcb	"SB"
	fdb	setbreakpoint		;Set breakpoint command.
	fcb	"TI"
	fdb	transfer_in		;Transferinput command. (INTEL HEX!)
	fcb	"TO"
	fdb	transfer_ot		;Transferoutput command. (INTEL HEX!)
	fcb	"HE"
	fdb	help			;Help command.
	fdb	0
	
helptext

	fcb	cr,lf,"This is the 6809 Scrumpel 8 monitor command helpmenu."
	fcb	cr,lf,cr,lf,cr,lf
	fcb	"AM <Alter>               AR <Alter register>  BC <Block copy>",cr,lf
        fcb	"BD <Block delete>        BI <Block insert>    CS <Clear screen>",cr,lf
	fcb	"FI <Find>                GO <Goto>            HD <Hexdump>",cr,lf
	fcb	"MO <set more>            PM <Preset>          SB <Set breakpoint>",cr,lf
	fcb	"SR <Show registers>      TI <TransferInput>   TO <TransferOutput>",cr,lf
	fcb	"HE <Help, this menu>      ? <Help, this menu>",cr,lf
	fcb	cr,lf,cr,lf
	fcb	"For more details see the 6809 monitor users guide.",cr,lf,0

help	jsr	ecls			;Clear display.
	ldx	#helptext		;Point X to helptext.
	jmp	ott			;Display the text.

goto	jsr	adresin			;Get address.
	jsr	in			;Wait for key.
	jsr	crlf			;Display crlf.
	ldy	#return			;Put returnaddress onto stack.
	pshs	y
	pshs	x			;Set the goto address onto stack.
	ldx	breakpoint		;Breakpoint set?
	beq	goto1			;No then normal goto.
	lda	regpsw
	tfr	a,cc			;Set psw.
	lda	rega			;No? get the registers.
	ldb	regb
	ldx	regx
	ldy	regy
goto1	rts

modifyreg

	clr	counter
	ldy	#reg_base		
reglp1	jsr	crlf
	lda	counter
	jsr	dregot
	lda	0,y
	pshs	a
	jsr	bytot
	lda	#space
	jsr	ot
	puls	a
	jsr	asciiot
	ldx	#alt2
	jsr	ott
reglp4	jsr	in
	cmpa	#cr
	beq	plusreg
	cmpa	#'-'
	beq	minreg
	jsr	alnibin
	bcs	reglp4
	jsr	albytin
	sta	0,y
	bra	plsreg
plusreg	lda	#'+'
	jsr	ot
plsreg	leay	1,y
	inc	counter
	cmpy	#regsp+2
	beq	modifyreg
	bra	reglp1

minreg	jsr	ot
	leay	-1,y
	dec	counter
	cmpy	#reg_base-1
	beq	modifyreg
	bra	reglp1


regtab	fcb	"ACCA=",0
	fcb	"ACCB=",0
	fcb	" CCR=",0
	fcb	" IXh=",0
	fcb	" IXl=",0
	fcb	" IYh=",0
	fcb	" IYl=",0
	fcb	"  DP=",0
	fcb	" UPh=",0
	fcb	" UPl=",0
	fcb	" SPH=",0
	fcb	" SPL=",0

dregot	ldx	#regtab			;Point X to register name table.
mesgot	pshs	b			;Save B.
	tsta				;Set CC flags according value in A.
	beq	mesdi			;0? Then print first in table.
meslp1	ldb	0,x+
	tstb
	bne	meslp1
	deca
	bne	meslp1
mesdi	jsr	ott
	puls	b			;Restore B.
	rts

asciiot	anda	#$7f			;Reset bit 8 from character.
	cmpa	#$7e			;Unprintable character?
	bcc	otpunt			;Print a '.'.
	cmpa	#' '			;Unprintable character?
	bcc	otkar			;No, print the character.
otpunt	lda	#'.'			;Print a '.'.
otkar	jmp	ot

alterg	jsr	ot			;Entry for G command.
	lda	#space			;Print a space.
	jsr	ot

alter	jsr	adresin			;Get address.
altl0	jsr	crlf
	jsr	xot			;Display address in X.
	lda	#space			;Display a space.
	jsr	ot
	lda	0,x			;Get byte pointed by address.
	pshs	a			;Save it.
	jsr	bytot			;Display byte.
	lda	#space			;Display space.
	jsr	ot
	puls	a			;Restore A.
	jsr	asciiot			;Display this as an ASCII character.
	pshs	x			;Save X.
	ldx	#alt2			;Display " : ".
	jsr	ott
	puls	x
altla	jsr	in			;Get first character.
	cmpa	#cr			;CR?
	beq	plus			;Yes? Then next address.
	cmpa	#'-'			;Is it a '-'?
	beq	min			;Yes? Then previous address.
	cmpa	#'G'			;Is it a 'G'?
	bne	alnxt			;No? Then check further.
	jmp	alterg			;Yes? Then ask for new address.
alnxt	cmpa	#'R'			;Is it a 'R'?
	beq	relative		;Yes? Then calculate relative address.
	cmpa	#"'"			;Is it a ' ?.
	beq	txtin			;A ' means text input.
	jsr	alnibin			;It must be a hex digit.
	bcs	altla			;No hex digit? Get first character again.
	jsr	albytin			;Get rest of byte.
	sta	0,x			;Store it on address pointed by X.
	bra	plus1			;Increment address.
plus	lda	#'+'			;Print a '+'.
	jsr	ot
plus1	lda	,x+			;Increment X by one.
	bra	altl0
min	jsr	ot			;Echo the character.
	leax	-1,x			;Decrement X by one.
altl0a	bra	altl0
txtin	jsr	ot			;Echo command.
	stx	buffer			;Save beginaddress into buffer.
txtinl	jsr	inul			;Get character in upper or lower case.
	cmpa	#$8			;A backspace pressed?
	bne	txtver			;Get next character.
	cmpx	buffer			;At begin of text?
	beq	txtinl			;Yes? Then do nothing get next character.
	pshs	x			;Save X.
	ldx	#bstxt			;Point X to backspace a character.
	jsr	ott			;Print it.
	puls	x			;Restore X.
	leax	-1,x			;Decrement X one character.
	bra	txtinl			;Get next character.
txtver	cmpa	#' '			;Below space?
	bcs	txtinl			;Yes? Do nothing with it.
	jsr	ot			;Echo character.
	cmpa	#"'"			;A ' means end of text.
	beq	altl0a
	sta	0,x+			;Store char and increment to next address.
	bra	txtinl			;Get next character.

relative

	jsr	ot			;Echo the command.
	leax	1,x			;Increment X.
	stx	buffer			;Save X in buffer.
	lda	#space			;Print a space.
	jsr	ot
	jsr	adresin			;Get branch to address.
	stx	tobuf			;Save it in tobuf.
	ldb	tobuf			;Calculate offset address.
	lda	tobuf+1
	suba	buffer+1
	sbcb	buffer
	beq	fwrd			;Forward relative jump.
	cmpb	#$ff
	beq	back			;Backwards relative jump.
offerr	ldx	#offseterr		;Point X to offseterror text.
	jsr	ott			;Print it.
	ldx	buffer			;Restore X from buffer.
	leax	-1,x			;Decrement it.
	jmp	altl0			;Try again.

back	tsta
	bpl	offerr
	bra	oexit
fwrd	tsta
	bmi	offerr
oexit	ldx	buffer			;Get original address.
	leax	-1,x			;Decrement it by one.
	sta	0,x			;Save calculated offset in address.
	pshs	a			;Save A.
	lda	#'='			;Print '='.
	jsr	ot
	puls	a			;Restore A.
	jsr	bytot			;Print the calculated offset.
	jmp	plus1			;Goto next address.

offseterr

	fcb	" Relative offset out of range.",0

bstxt	fcb	bs,space,bs,0

hdump	jsr	adresin			;Get beginaddress to dump.
hloop	jsr	crlf			;New line.
	pshs	x			;Save X.
	jsr	xot			;Print the address.
	lda	#space			;Print a space.
	jsr	ot
	ldb	#$8			;8 double bytes to print.
hloop1	lda	#space			;Print a space first.
	jsr	ot
	lda	0,x+			;Get first byte.
	jsr	bytot			;Print value.
	lda	0,x+			;Get second byte.
	jsr	bytot			;Print value.
	decb				
	bne	hloop1			;8 done? No then print next double byte.
	puls	x			;Restore X.
	lda	#space			;Print a space.
	jsr	ot
	ldb	#$10			;16 Characters to print.
hloop2	lda	0,x+			;Get character.
	jsr	asciiot			;Go print it.
	decb				;16 done?.
	bne	hloop2			;No, print next character.
	lda	consolebreak		;Check for break.
	bne	hloop
	lda	#$ff			;Stop dumping.
	sta	consolebreak
	rts

preset	jsr	adresin			;Get beginaddress.
	pshs	x			;Save it.
	lda	#space			;Print a space.
	jsr	ot
	jsr	adresin			;Get endaddress.
	leax	1,x			;Increment it by one.
	stx	buffer			;Save it in buffer.
	puls	x			;Get beginaddress back.
	lda	#space			;Print a space.
	jsr	ot
	jsr	bytin			;Get value of presetbyte.
presl1	sta	0,x+			;Store it on address.
	cmpx	buffer			;End of block reached?
	bne	presl1			;No? fill next byte.
	rts				;Yes? Done.

delete	jsr	bytin			;Get number of bytes to delete.
	sta	movscr			;Save it.
	lda	#space			;Print a space.
	jsr	ot
	jsr	adresin			;Get beginaddress of block to delete.
	stx	buffer			;Save it in buffer.
	lda	#space			;Print a space.
	jsr	ot
	jsr	adresin			;Get endaddress of block to delete.
	leax	1,x			;Increment it by one.
	stx	tobuf			;Save in tobuf.
	ldx	buffer			;Point X to beginaddres in buffer.
dloop	pshs	x			;Save it.
	ldb	movscr			;Load the offset.
	abx				;Add it to X.
	lda	0,x			;Get the byte plus the offset.
	puls	x			;Get X back.
	sta	0,x+			;Store it onto address minus offset.
	cmpx	tobuf			;End of block reached?
	bne	dloop			;No? Continue with delete.
	rts				;Yes? Done

insert	jsr	bytin			;Get number of bytes to insert.
	sta	movscr			;Save it.
	lda	#space			;Print a space.
	jsr	ot
	jsr	adresin			;Get beginaddress of block to insert.
	leax	-1,x			;Decrement it by one.
	stx	tobuf			;Save in tobuf.
	lda	#space			;Print a space.
	jsr	ot
	jsr	adresin			;Get endaddress of block to insert.
iloop	lda	0,x			;Get byte to delete
	pshs	x			;Save X.
	ldb	movscr			;Get offset.
	abx				;Add to X.
	sta	0,x			;Store it.
	puls	x			;Restore X.
	leax	-1,x			;Decrement X by one.
	cmpx	tobuf			;End of block reached?
	bne	iloop			;No? Continue with insert.
	rts				;Yes? Done.

copy	jsr	adresin			;Get beginaddress to copy from.
	pshs	x			;Save X.
	lda	#space			;Print a space.
	jsr	ot
	jsr	adresin			;Get endaddress to copy from.
	leax	1,x			;Decrement X by one.
	stx	tobuf			;Save it in tobuf.
	lda	#space			;Print a space.
	jsr	ot
	jsr	adresin			;Get destination address to copy to.
	pshs	x			;Save X.
	puls	y			;Copy into Y.
	puls	x			;Get beginaddress.
cloop	lda	0,x+			;Load the byte pointed by X and increment X.
	sta	0,y+			;Store the byte pointed by Y and increment Y.
	cmpx	tobuf			;End reached?
	bne	cloop			;No? Copy next byte.
	rts				;Yes? Done.

chksum	adda	crc			;Calculate checksum
	sta	crc			;and store it in CRC
	rts

transfer_in

	jsr	crlf			;Print crlf.
trl1	jsr	in			;Wait for a ':'.
	cmpa	#':'
	bne	trl1
	jsr	ot			;Echo the character.
	jsr	bytin			;Get length of line.
	beq	trend			;0? This means end of IHEX.
	sta	teller			;Save number of bytes in teller.
	clr	crc			;Clear CRC.
	jsr	chksum			;Calculate CRC.
	jsr	bytin			;Get high byte of address.
	sta	buffer
	jsr	chksum			;Calculate CRC.
	jsr	bytin			;Get low byte of address. 
	sta	buffer+1
	jsr	chksum			;Calculate CRC.
	ldx	buffer			;Buffer contains address, put in X.
	jsr	bytin			;Get control byte.
	jsr	chksum			;Calculate CRC.
trl0	jsr	bytin			;Get byte to store.
	sta	0,x+			;Store it in memory and increment X.
	jsr	chksum			;Calculate CRC.
	dec	teller			;All done?
	bne	trl0			;No? Get next byte.
	jsr	bytin			;Get CRC byte.
	nega
	cmpa	crc			;Check CRC.
	beq	transfer_in		;The same? Then get next line.
chkerr	ldx	#transfererr		;No? Print error message.
	jmp	ott
trend	jsr	in			;Wait for end of line.
	cmpa	#cr			
	bne	trend
	rts				;Done.

transfererr

	fcb	cr,lf,"CRC error in Intel-hex transfer.",0

transfer_ot

	jsr	adresin			;Get begin address.
	pshs	x			;Save it (X).
	lda	#space			;Print a space.
	jsr	ot
	jsr	adresin			;Get number of lines to sent.
	stx	tobuf			;Store it in tobuf.
	puls	x			;Get beginaddress (X).
tril1	clr	crc			;Clear CRC.
	jsr	crlf			;Print cr/lf.
	stx	buffer			;Save X in buffer.
	lda	#':'			;Print ':'.
	jsr	ot
	lda	#$10			;A line contains 16 bytes.
	sta	teller			;Store it in teller.
	jsr	bytot			;Print the number of bytes.
	jsr	chksum			;Calculate CRC.
	lda	buffer			;Print high byte of address.
	jsr	bytot
	jsr	chksum			;Calculate CRC.
	lda	buffer+1		;Print low byte of address.
	jsr	bytot
	jsr	chksum			;Calculate CRC.
	clra				
	jsr	bytot			;Sent a 00 byte.
	jsr	chksum			;Calculate CRC.
tril0	lda	0,x+			;Get byte at address (X).
	jsr	bytot			;Print the byte.
	jsr	chksum			;Calculate CRC.
	dec	teller			;Decrement teller.
	bne	tril0			;All done? No do next byte.
	lda	crc			;Yes? Calculate CRC.
	nega				;Do a negate.
	jsr	bytot			;Print result.
	pshs	x			;Save X.
	ldx	tobuf			;Get number of lines.
	leax	-1,x			;Decrement number of lines.
	beq	trend1			;If equals 0 then end transfer.
	stx	tobuf			;Save number of lines in tobuf.
	puls	x			;Restore X.
	bra	tril1			;Print next line.
trend1	puls	x			;Restore X.
	ldx	#trendt			;Print end of transfer string.
	jmp	ott

trendt	fcb	cr,lf,":00000001FF",cr,lf,0

snuffel	jsr	adresin			;Get beginaddress to search in.
	pshs	x			;Save it.
	lda	#space			;Print a space.
	jsr	ot
	jsr	adresin			;Get endaddres to search in.
	leax	1,x			;Increment it by one.
	stx	tobuf			;Save it in tobuf.
	puls	x			;Get beginaddress in X. 
	lda	#space			;Print a space.
	jsr	ot
	jsr	bytin			;Get the byte to search for.

snuffel_verder

	cmpx	tobuf			;Reached the end of block?
	beq	snuffel_niet_gevonden	;Yes? Nothing found.
	ldb	0,x+			;Get byte in B.
	pshs	b			;CBA, Compare A with B.
	cmpa	,s+
	bne	snuffel_verder		;Not equal? Get next byte.
	pshs	x			;Save X.
	ldx	#snuffel_gevonden_text	;Print found the byte.
	jsr	ott
	puls	x			;Restore X.
	leax	-1,x			;Decrement the address.
	jmp	xot			;Print the address where byte is found.

snuffel_niet_gevonden

	ldx	#snuffel_niet_gevonden_text
	jmp	ott			;Print nothing found.

snuffel_gevonden_text

	fcb	" found at : ",0

snuffel_niet_gevonden_text

	fcb	" not found in given memory block.",0

;	For more information about this monitor contact:
;	Nick brok Berliozlaan 1 5654 SM Eindhoven.

	dephase
	end
