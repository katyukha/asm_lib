jmp ___end_asm_lib___


;				FUNCTIONS menu_routine
;*------------------------------------------------------------------------------
__menu_create:
	;creates the menu structure
	;[in] dx - receives addres of buffer
	;[in] si - receives addres of menu header
	push di
	mov di, dx
	mov byte [di], 0	;first byte reserved of number of item selected
	mov word [di+1], si	;saving addres of caption
	mov byte [di+3], 0	;4th byte determines how many items are in this menu
	pop di
	ret
;*------------------------------------------------------------------------------
__menu_add_item:
	; adds one more menu item to the menus structure
	; [in] dx - address of menu structure
	; [in] bx - address of label of item
	; [in] si - address of function to call when item selected
	push di
	push cx
	push ax
	
	mov di, dx
	add di, 3		;skiping first 3 bytes
	xor cx, cx
	mov byte cl, [di]
	inc byte [di]		;incrementing count of items in menu
	inc di			;skiping byte that holds count of items in menu
	
	push dx
	mov ax, 4		;any item shoul teke 4 bytes in menu structure
	mul cx
	pop dx			;multiplication changes dx register
	
	add di,ax		;skiping all items in menu
	
	mov word [di], bx	;setting label
	add di,2
	mov word [di], si	;setting caller
	
	pop ax
	pop cx
	pop di
	ret
;*------------------------------------------------------------------------------
__menu_get_item:
	; returns the address of the menu item structure by given menu structure and item id
	; [in] dx - addres of menu strucure
	; [in] al - item id (pos in structure)
	; [out] di - addres of menu item structure	
	push cx
	push ax
	
	mov di, dx
	add di, 4		;skiping first 3 bytes
	
	xor cx,cx
	mov cl, al			
	mov ax, 4		;any item shoul teke 4 bytes in menu structure
	push dx	
	mul cx	
	pop dx
	
	add di,ax		;skiping all items in pos < al in menu			
	pop ax
	pop cx	
	ret
;*------------------------------------------------------------------------------
__menu_print:
	; prints the menu to the display
	; [in] dx - address of menu structure
	push di
	push cx
	push dx	
	push ax
	
	call __clear_scr		;cleanes screen
	
	mov di,dx
	mov byte al, [di]		;id of selected item
	inc di				
		
	mov dx, _______________________menu_spacer
	call __print_str
	mov word dx, [di]		;printing menu label
	call __print_str	
	call __print_nl
		
	add di,2
	cmp byte [di],0			;checkin count of items in menu
	je ___menu_print_exit
	
	xor cx, cx		
	mov byte cl, [di]		;setting loop counter to count of items in menu
	inc di				;
	mov ah,0			;id of current item
	_____start_menu_print_loop:
		cmp al,ah
		je ___________________selected
		mov dx, _______________________menu_spacer		
		___________________selected:
		jne ___________________print_item
		mov dx,_______________________menu_selector
		___________________print_item:
		call __print_str
		mov word dx, [di]
		call __print_str
		call __print_nl
		
		add di, 4		;going to next item to print
		inc ah			;increasing id of current item
	loop _____start_menu_print_loop
	___menu_print_exit:
	pop ax 
	pop dx	
	pop cx
	pop di
	ret
	
	_______________________menu_selector:		db	"   -> $"
	_______________________menu_spacer:		db	"      $"
;*------------------------------------------------------------------------------
___menu_key_handler:
	; handles basic keys that allows browsing throught the menu
	; [in] dx - menu structure
	; [in] ax - key code
	push di
	push bx	
	push ax
	
	call __read_key
	
	mov di, dx	
	mov bl, [di]
	cmp al,0
	jne ___menu_key_handler_menu_enter
	
	;-------------
	___menu_key_handler_menu_up:
	cmp ah, 72						
	jne ___menu_key_handler_menu_down	 		
	cmp bl, 0			
	je ___menu_key_handler_exit
	dec byte [di]
	jmp ___menu_key_handler_exit	
	;-------------
	___menu_key_handler_menu_down:
	cmp ah, 80
	jne ___menu_key_handler_menu_enter
	
	add di, 3
	inc bl
	cmp byte bl, [di]
	je ___menu_key_handler_exit
	mov di, dx
	inc byte [di]
	jmp ___menu_key_handler_exit
	;-------------
	___menu_key_handler_menu_enter:	
	cmp al, 13
	jne ___menu_key_handler_menu_esc
	mov al,bl
	call __menu_get_item
	call __clear_scr	
	pusha
	mov word si, [di+2]
	call si
	popa
	call __read_key
	jmp ___menu_key_handler_exit
	;-------------
	___menu_key_handler_menu_esc:
	cmp al, 27
	jne ___menu_key_handler_exit
	mov byte [di], 128
	;-------------
	___menu_key_handler_exit:
	pop ax
	pop bx	
	pop di	
	ret

__start_menu_loop:
	;starts main loop which controls browsing throught the menu
	; [in] dx - address of menu structure
	push di
	mov di, dx	
	___menu_main_loop_begin:
		call __menu_print
		call ___menu_key_handler
	
		cmp byte [di], 128
	jne ___menu_main_loop_begin	
	pop di
	ret		

;				FUNCTIONS VIDEO CURSOR
;*------------------------------------------------------------------------------
__set_cur_pos:
	;уст. позицию курсора. установка на строку 25 делает курсор невидимым.
	;DH,DL = строка, колонка (считая от 0)
	push ax
	push bx
	mov ah,02h
	mov bh, 0
	int 10h
	pop bx
	pop ax
	ret
__get_cur_pos:
	;читать позицию курсора
	;выход:  DH,DL = текущие строка,колонка курсора
	push ax
	push bx
	push cx
	mov bh, 0
	mov ah, 03h
	int 10h
	pop cx
	pop bx
	pop ax
	ret
__write_symb_in_cur_pos:
	;писать символ в текущей позиции курсора
	;AL = записываемый символ
	push ax
	push bx
	push cx
	mov ah,09h
	mov bh,0
	mov cx,1
	int 10h
	pop ax
	pop bx
	pop cx
	ret


;				FUNCTIONS MATRIX
;*------------------------------------------------------------------------------
__matrix_create:
	;creates the matrix
	; [in] bl - rows
	; [in] bh - cols
	; [in] dx - buffer for matrix
	push di
	mov di, dx
	mov word [di], bx		
	pop di
	ret
__matrix_get:
	;returns element of matrix
	;[in] dx - matrix	
	;[in] bl - row
	;[in] bh - col
	;[out] al - result
	push di	
	push cx	
	mov di,dx
	mov word cx, [di]
	
	xor ax,ax
	mov al, cl		;getting offcet
	push dx
	mul bl
	pop dx
	xor cx,cx
	mov cl, bh
	add ax, cx	
	add ax, 2		
	
	add di, ax
	mov byte al, [di]	;getting element	
	
	pop cx
	pop di	
	ret

__matrix_set:
	;sets element of matrix to value in al
	;[in] dx - matrix	
	;[in] bl - row
	;[in] bh - col
	;[out] al - result
	push di
	push cx	
	mov di,dx
	mov word cx, [di]
	
	push ax
	xor ax,ax
	mov al, cl		;getting offcet
	push dx
	mul bl
	pop dx
	xor cx,cx
	mov cl, bh
	add ax, cx	
	add ax, 2			
	add di, ax
	pop ax
	
	mov byte [di], al	;getting element	
	
	pop cx
	pop di	
	ret

__matrix_loop:
	;this function starts and holds iterration of elements of matrix
	;[in] dx - address of initialized matrix	
	;[in] si - address of function to call. this function should operate winth registrs:
	;			bh - col 
	;			bl - row
	;			al - current element.
	;		after execution of function value of al will be saved to a matrix
	push di
	push cx
	push ax
	push bx
	mov di,dx	
	mov word ax, [di]
	
	xor cx,cx
	mov cl, al
	mov bl, 0
	___matrix_loop_main:
		push cx
		xor cx, cx
		
		mov bh,0
		mov cl, ah
		___matrix_loop_second:
			;push cx
			;push di			
			;push ax						
			;push si
			pusha							
			call __matrix_get
			push dx
			push bx
			
			cmp bh, 0
			jg _____call_sub			
			mov dl, -1
			add dl, 1
			_____call_sub:
			call si			;call external function
						
			pop bx			
			pop dx
			call __matrix_set
			popa
			;pop si
			;pop ax
			;pop di
			;pop cx
			
			inc bh	
			call __matrix_check_nl
		loop ___matrix_loop_second
		
		pop cx
	loop ___matrix_loop_main
	
	pop bx
	pop ax
	pop cx
	pop di
	ret
	
__matrix_check_nl:
	;receives in dx matrix in bx coordinates
	;corrects coordinates if new line of matrix was found
	push di
	push ax
	mov di, dx
	mov word ax, [di]
	
	cmp ah, bh
	jg ___matrix_new_line_end
	mov bh, 0
	inc bl		
	___matrix_new_line_end:
	pop ax
	pop di
	ret

;				BCD FUNCTIONS
;*------------------------------------------------------------------------------
__decode_bcd:
	;декодує al з BCD формату так що перша цифра поміщається в ah а друга в al
	;[in]  al - BCD format value
	;[out] ah - first digit of number (bin representation)
	;[out] al - second digit of number (bin representation)
	push bx
	mov bl, al
	and bl, 0F0h
	shr bl, 4 	
	mov ah, bl
	and al, 0Fh
	pop bx
	ret	
;*------------------------------------------------------------------------------
__decode_bcd_symb:
	;те саме що попереднья функція, тільки ще "перетворює цифру в символ"
	;[in]  al - BCD format value
	;[out] ah - first digit of number (symbolic representation)
	;[out] al - second digit of number (symbolic representation)
	call __decode_bcd		
	add ah, '0'
	add al, '0'	
	ret
;*------------------------------------------------------------------------------
__bcd_to_bin:
	;конвертує bcd формат у двійковий приймає al з числом у bcd форматі
	;converts BCD format to binary value	
	;[in]  al - BCD format value
	;[out] al - bin value
	push bx
	mov bl, al
	and al, 0F0h		;отримуємо першу цифру і множимо її на десять
	shr al, 4	
	mov bh, 10
	push dx
	mul bh
	pop dx
	and bl, 0Fh
	add al,bl	
	pop bx
	ret
;*------------------------------------------------------------------------------
__bin_to_bcd:
	;приймає в al двійкове число і перекодовує його в bcd формат....
	;converts binary value to BCD format
	;[in]  al - binary value
	;[out] al - BCD format value
	push bx	
	xor ah, ah
	mov bh, 10
	div byte bh	
	shl al, 4
	or al,ah	
	pop bx
	ret
;*------------------------------------------------------------------------------
__byte_to_str:
	;[in] al - byte to encode 
	;[in] dx - buffer for encoded al (2 bytes)
	;in first two bytes will be placed two digits of number in al
	;converts al to string
	push ax
	push di
	mov di, dx
	call __bin_to_bcd
	call __decode_bcd_symb
	mov [di], ah	
	mov [di+1], al
	pop di	
	pop ax
	ret
;*------------------------------------------------------------------------------
__str_to_byte:
	;[in] dx - buffer for decode (first 2 bytes are effective)
	;[out] al - result (decoded number)
	;converts string (2 symbols) to integer(byte)
	push bx
	push di
	
	mov di, dx	
	xor ax,ax
	mov byte al, [di]
	sub al, '0'
	cmp byte [di+1], 13
	je ___str_to_byte_end
	push dx
	mov bh, 10
	mul bh
	pop dx
	mov byte bl, [di+1]
	sub bl, '0'
	add al, bl
	
	___str_to_byte_end:
	pop di
	pop bx
	ret
;*------------------------------------------------------------------------------
	
;				FUNCTIONS GENERAL
;*------------------------------------------------------------------------------
__read_key:
	;читать (ожидать) следующую нажатую клавишу
        ;    выход: AL = ASCII символ (если AL=0, AH содержит расширенный код ASCII )
        ;           AH = сканкод  или расширенный код ASCII
	mov ah,00h
	int 16h	
	ret
;*------------------------------------------------------------------------------
__key_pressed:
	;checks whether a key was pressed
	;[out] ZF - 0 if key was pressed and 1 if key wasn't pressed
	;[out] AX - key that was pressed (but key will not be popped out from the key queue
	mov ah,00h
	int 16h
	ret
;*------------------------------------------------------------------------------
__clear_scr:
	;cleanes screen resetting graphic mode
	push ax
	mov al,2
        mov ah,00h        					;очистка екрана
        int 10h
	pop ax
	ret
;*------------------------------------------------------------------------------
__read_str:
	;in al receives how many bytes to read
	;in dx receives address of buffer
	push di
	mov di,dx
	mov [di], al
	mov ah,0ah		;bufferized input
	int 21h			;DOS interrupt
	pop di
	ret
;*------------------------------------------------------------------------------
__read_symb:
	;returns entered symbol in al	
	mov ah, 01
	int 21h
	ret
;*------------------------------------------------------------------------------
__print_symb:
	;prints symbol passed to dl register dl
	push ax
	mov ah,02h
	int 21h
	pop ax
	ret
;*------------------------------------------------------------------------------
__print_symb_bios:
	;prints symbol using 10h bios interrupt
	;prints unprintable symbols
	; [in] al - symbol to print
	push bx
	push cx				;saving registers
	push dx
	
	mov bh, 0			; select video page
	
	mov ah, 09h			; print symbol
	mov cx, 1			; one symbol
	mov bl, 10			; let it to be lightgreen
	int 10h				
	
	mov ah, 03h			; getting position of cursor 
	int 10h
	
	inc dl				; move to next column
	cmp dl, 50			; if it less than 24
	jle __end			; nothing to do else
	mov dl, 0			; else make it zero
	inc dh				; move to next line
	cmp dh, 24			; if line pos less then 24
	jle __end			; then nothing to do 
	mov dh,0			; else return to the begining
	
	__end:
	mov ah,02h			; save new position of cursor
	int 10h
	
	pop dx
	pop cx				; restore registers
	pop bx
		
	
;*------------------------------------------------------------------------------
__print_str:
	;друкує стрічку адреса якої записана в dx, стрічка має закінчуватись доларом 
	push ax
	mov ah, 09h
	int 21h
	pop ax
	ret
;*------------------------------------------------------------------------------
__print_nl:
	;друкує перехід на нову стрічку
	push ax
	push dx
	mov dx, _____new_line
	mov ah, 09h
	int 21h
	pop dx
	pop ax
	ret
	_____new_line:	db 10,13,"$"
;*------------------------------------------------------------------------------
__set_last_char:
	;adds al symbol to the end of string replacing its last symbol
	;(only for strings in dos format(first two bytes contains how many bytes
	;have to be read and how many byte were really read)
	; [in] al - char to set as last
	; [in] dx - string in dos format
	
	push di
	push bx
	mov di, dx
	xor bx, bx
	mov bl, [di+1]
	add di, bx
	add di, 3
	mov [di], al
	pop bx
	pop di
	ret
;*------------------------------------------------------------------------------
;				TIMER FUNCTIONS
;*------------------------------------------------------------------------------
__delay:
	;stops execution of program for cx seconds
	; [in] cx - receives seconds to delay
	push ax
	push bx
	push dx
	delay_start__:			;
	push cx
	mov ah,0			;zero function of timer interrupt. returns in cx and dx count of tics
	int 1ah				;timer interrupt
	mov bx,dx			;save tics(lower part) into bx	
	add bx,18			;add to saved tics 1 second
	second__:			
		int 1ah			;get tics again
		cmp bx,dx		;while it less then bx
	jge second__			;
	pop cx
	loop delay_start__		;repeating cx times
	pop dx
	pop bx
	pop ax
	ret
;*------------------------------------------------------------------------------
__get_tics:
	;gets tics from timer
	;[out] cx - higher part of value
	;[out] dx - lower part of value
	push ax
	mov ah, 0
	int 1ah
	pop ax
	ret
;*------------------------------------------------------------------------------
__timer_start:
	;Simulates timer
	;[in]  si - function to execute
	;[in]  dx - time in seconds
	;TODO : create timer emulation mechanism
	
	ret
	
	___timer_time:		dw	0		;saving time 
	___timer_func:		dw	0		;address of function which will be called when timer will be triggered
;*------------------------------------------------------------------------------
__exit:
	;exits the program
	mov ah,4ch
	int 21h
;*------------------------------------------------------------------------------
___end_asm_lib___:
