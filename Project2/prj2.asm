.model small

include rtos8086\\rtos8086.inc
includelib rtos8086\\rtos8086.lib

;; -------------------------------------
;   User Data
;; -------------------------------------
.data
    counter_30min_div1s dw 0
    digits db 8 dup (0)
    selected db 0
    keyboard_task_handle dw 0
    timer_30_task_handle dw 0
.const
    PORT_8259 equ 290h
    PORT_8259_EVEN equ PORT_8259+0
    PORT_8259_ODD equ PORT_8259+1

    PORT_8253 equ 298h
    PORT_8253_0 equ PORT_8253+0
    PORT_8253_1 equ PORT_8253+1
    PORT_8253_2 equ PORT_8253+2
    PORT_8253_CTRL equ PORT_8253+3

    PORT_8255 equ 288h
    PORT_8255_A equ PORT_8255+0
    PORT_8255_B equ PORT_8255+1
    PORT_8255_C equ PORT_8255+2
    PORT_8255_CTRL equ PORT_8255+3
    DIGIT_2_LED db 0C0H, 0F9H, 0A4H, 0B0H, 99H, 92H, 82H, 0F8H, 80H, 90H
.code
;; -------------------------------------
;   User Code
;; -------------------------------------

delay_cx_nop proc near
    delay_cx_nop_loop: nop
    loop delay_cx_nop_loop
    
    ret
delay_cx_nop endp

x8255A_setup proc near
    mov dx, PORT_8255_CTRL
    mov al, 10000000b
    out dx, al

    ret
x8255A_setup endp

x8259A_setup proc near
    mov dx, PORT_8259_EVEN
    mov al, 00010010b
    out dx, al

    mov dx, PORT_8259_ODD
    mov al, 0ah
    out dx, al

    ret
x8259A_setup endp

x8253A_setup proc near
    mov dx, PORT_8253_CTRL
    mov al, 00110110b
    out dx, al

    mov dx, PORT_8253_0
    mov ax, 30000
    out dx, al
    mov al, ah
    out dx, al

    mov dx, PORT_8253_CTRL
    mov al, 01110110b
    out dx, al

    mov dx, PORT_8253_1
    mov ax, 60000
    out dx, al
    mov al, ah
    out dx, al

    mov dx, PORT_8253_CTRL
    mov al, 10110110b
    out dx, al

    mov dx, PORT_8253_1
    mov ax, 1000
    out dx, al
    mov al, ah
    out dx, al

    ret
x8253A_setup endp

keyboard_setup proc near
    lea cx, keyboard_task
    mov dl, 0fdh
    call add_task_table
    mov [keyboard_task_handle], bx

    ret
keyboard_setup endp

keyboard_task proc RTOS_FUNC_TYPE
    mov bx, [keyboard_task_handle]
    call vTaskSuspend

    keyboard_task_loop:


        call get_keyboard_signal
        cmp al, byte ptr '0'
        jne if_0_is_input_not

        if_0_is_input:
            mov bx, [timer_30_task_handle]
            call vTaskResume
            xor al, al
            call set_keyboard_signal
            mov bx, [keyboard_task_handle]
            call vTaskSuspend
        if_0_is_input_not:
            nop

        mov ax, 50
        call vTaskDelay
        jmp keyboard_task_loop

    call get_task_current
    call vTaskDelete
    ret
keyboard_task endp

timer_30_setup proc near
    lea cx, display_task
    mov dl, 0feh
    call add_task_table
    mov [timer_30_task_handle], bx

    ret
timer_30_setup endp

reset_counter_30 proc near
    mov digits[0], 3
    mov digits[1], 0
    mov digits[2], 0
    mov digits[3], 0
    mov word ptr [counter_30min_div1s], 0

    ret
reset_counter_30 endp

dec_digits_30 proc near    
    sub digits[3], 1
    jns if_digits_not_neg
    mov digits[3], 9
    sub digits[2], 1
    jns if_digits_not_neg
    mov digits[2], 5
    sub digits[1], 1
    jns if_digits_not_neg
    mov digits[1], 9
    sub digits[0], 1
    jns if_digits_not_neg
    mov digits[0], 2
    if_digits_not_neg:
        inc word ptr [counter_30min_div1s]

    ret
dec_digits_30 endp

timer_30_task proc RTOS_FUNC_TYPE

    call reset_counter_30

    timer_30_loop:
        
        cmp counter_30min_div1s, 1800
        jne if_counter_30min_not

        if_counter_30min:
            mov bx, [keyboard_task_handle]
            call vTaskResume
            call get_task_current
            call vTaskSuspend
            call reset_counter_30
        if_counter_30min_not:
            call dec_digits_30

        mov ax, 1000
        call vTaskDelay
        jmp timer_30_loop

    call get_task_current
    call vTaskDelete
    ret
timer_30_task endp

display_setup proc near
    lea cx, display_task
    mov dl, 0fch
    call add_task_table
    call add_idle_task_table

    ret
display_setup endp

display_task proc RTOS_FUNC_TYPE

    display_task_loop:
        mov cx, 8
        mov ah, 11111110b ; chip select
        display_loop:
            mov al, 0ffh
            mov dx, PORT_8255_B
            out dx, al

            mov al, cl
            dec al
            mov dl, al
            lea bx, digits
            xlatb ; which digit
            lea bx, DIGIT_2_LED
            xlatb ; what to display
            cmp dl, [selected] ; if the current is selected
            jne if_cur_is_selected_else
            if_cur_is_selected:
                and al, 7h ; add the dot
            if_cur_is_selected_else:
                nop

            mov dx, PORT_8255_A
            out dx, al
            
            mov al, ah
            mov dx, PORT_8255_B
            out dx, al
            
            ror ah, 1
            push cx
            mov cx, 10
            call delay_cx_nop
            pop cx
            loop display_loop

        mov ax, 100
        call vTaskDelay
        jmp display_task_loop

    call get_task_current
    call vTaskDelete
    ret
display_task endp

app_main proc near
    call display_setup
    call keyboard_setup
    call timer_30_setup

    ret
app_main endp

pre_main_setup proc near
    call x8255A_setup
    call x8253A_setup
    call x8259A_setup

    ret
pre_main_setup endp

post_main_setup proc near
    ret
post_main_setup endp

end main