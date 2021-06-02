; zxmmc.asm - an experimental FID for the ZXMMC interface
;
; Public-domain software by Paul Osmialowski <pawelo@king.net.pl>
;
; Constraints:
; - slot 0 only
; - up to 8GB MMC cards (cyls/2/128 C/H/S geometry only)
; - PLUSIDEDOS partition table only (in sector 0 or sector 128)
; - partition table must be terminated with 'UNUSED' partition (IDEDOS type 0)
; - up to three partitions (searched by name), static drive letters assignment:
;   C: CP/M
;   D: SOFTWARE
;   E: DEVEL
; - searching partitions by name is case sensitive (upper case names only!)
; - searching through 16MB '+3DOS' (IDEDOS type 3) partitions only
; - giving up with searching on the first 'UNUSED' partition (IDEDOS type 0)
; - can fall into an infinite loop if there are more than 2040 partitions
;
; Build by:
;   first on UNIX:
;           pasmo --prl zxmmc.asm zxmmc.fid
;   then on CP/M:
;           fidcsum zxmmc.fid
;   (get fidcsum.com from https://www.seasip.info/Cpm/software/fidcsum.com)
;
VERSION     equ 0x100
SECTOR_SIZE equ 512
;
; CPLD registers:
TXREG       equ 0x7f
RXREG       equ 0x7f
KEMPSTON    equ 0x1f
STATUS      equ 0x5f
SPI_PORT    equ 0x57
OUT_PORT    equ 0x77
;
MMC_0       equ 1
MMC_1       equ 2
;
; MMC commands:
MMC_GO_IDLE_STATE         equ 0x40
MMC_SEND_OP_COND          equ 0x41
MMC_READ_CID              equ 0x4a
MMC_SET_BLOCK_SIZE        equ 0x50
MMC_READ_SINGLE_BLOCK     equ 0x51
MMC_READ_MULTIPLE_BLOCKS  equ 0x52
MMC_TERMINATE_MULTI_READ  equ 0x4c
MMC_WRITE_SINGLE_BLOCK    equ 0x58
MMC_WRITE_MULTIPLE_BLOCKS equ 0x59
MMC_STOP_TRAN             equ 0xfd
;
; +3 supervisor calls:
SVC_BANK_05               equ $ + 0xfe00
SVC_BANK_68               equ $ + 0xfe01
SVC_CATCHUP               equ $ + 0xfe02
SVC_SCB                   equ $ + 0xfe03
SVC_C_HOOK                equ $ + 0xfe04
SVC_D_HOOK                equ $ + 0xfe05
SVC_D_CHANGED             equ $ + 0xfe06
SVC_ALLOCATE              equ $ + 0xfe07
SVC_MAX_ALLOCATE          equ $ + 0xfe08
SVC_DEALLOCATE            equ $ + 0xfe09
SVC_C_FIND                equ $ + 0xfe0a
;
; CP/M drives:
DRIVE_A                   equ 0x00
DRIVE_B                   equ 0x01
DRIVE_C                   equ 0x02
DRIVE_D                   equ 0x03
DRIVE_E                   equ 0x04
DRIVE_F                   equ 0x05
DRIVE_G                   equ 0x06
DRIVE_H                   equ 0x07
DRIVE_I                   equ 0x08
DRIVE_J                   equ 0x09
DRIVE_K                   equ 0x0a
DRIVE_L                   equ 0x0b
DRIVE_M                   equ 0x0c
DRIVE_N                   equ 0x0d
DRIVE_O                   equ 0x0e
DRIVE_P                   equ 0x0f
DRIVE_NEXT                equ 0xff
;
; IDEDOS partition types:
IDEDOS_TYPE_UNUSED        equ 0x00
IDEDOS_TYPE_PARTITION_TBL equ 0x01
IDEDOS_TYPE_SWAP          equ 0x02
IDEDOS_TYPE_PLUS3DOS      equ 0x03
IDEDOS_TYPE_BAD           equ 0xfe
IDEDOS_TYPE_FREE          equ 0xff
;
IDEDOS_PARTITIONS_PER_SECTOR equ 8
;
; IDEDOS partition entry:
IDEDOS_PE_TYPE            equ 0x10
IDEDOS_PE_START_CYL_LO    equ 0x11
IDEDOS_PE_START_CYL_HI    equ 0x12
IDEDOS_PE_START_HEAD      equ 0x13
IDEDOS_PE_END_CYL_LO      equ 0x14
IDEDOS_PE_END_CYL_HI      equ 0x15
IDEDOS_PE_END_HEAD        equ 0x16
;
IDEDOS_PE_XDPB_OFFSET     equ 0x20
IDEDOS_PE_SIZE            equ 0x40
;
; IDEDOS partition table info:
IDEDOS_PT_NUM_CYLS_LO     equ 0x20
IDEDOS_PT_NUM_CYLS_HI     equ 0x21
IDEDOS_PT_NUM_HEADS       equ 0x22
IDEDOS_PT_SECT_PER_TRACK  equ 0x23
IDEDOS_PT_SECT_PER_CYL_LO equ 0x24
IDEDOS_PT_SECT_PER_CYL_HI equ 0x25
IDEDOS_PT_MAX_PART_LO     equ 0x26
IDEDOS_PT_MAX_PART_HI     equ 0x27
;
ZXMMC_CYLS_PER_16MB_PARTITION_MINUS_ONE equ (128 - 1)
;
fidheader:
    jp  fidems
    db  'SPECTRUM' ; name
    db  'FID'      ; type
    dw  VERSION    ; version
    dw  0          ; checksum
    db  0          ; can start anywhere
    db  0          ; can end anywhere
    db  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ; reserved
;
partition_1:
    ;   0123456789abcdef
    db 'CP/M            '
partition_2:
    ;   0123456789abcdef
    db 'SOFTWARE        '
partition_3:
    ;   0123456789abcdef
    db 'DEVEL           '
;
jumpblock:
    jp  fiddlogon
    jp  fiddread
    jp  fiddwrite
    jp  fiddflush
    jp  fiddmess
;
msg_zxmmc:
    db  'ZC SD v'
    db  (high VERSION) + '0', '.', (low VERSION) + '0'
    db  0xd, 0xa, 0xff
;
msg_nozxmmc:
    db  'ZC SD device inaccessible'
    db  0xd, 0xa, 0xff
;
valid_dpb:
    dw  0x0200 ; SPI: 128-byte records per track; 128 sectors per logical track
    db  0x06   ; BSH: log2(block_size / 128); block_size = 8192
    db  0x3f   ; BLM: (block_size / 128) - 1
    db  0x03   ; EXM: extent mask
    dw  0x07f7 ; DSM: last block number, blocks_on_disk - 1
    dw  0x01ff ; DRM: last dir entry, dir_entries - 1; dir_entries = 512
    db  0xc0   ; AL0
    db  0x00   ; AL1
    dw  0x8000 ; CKS: checksum vector size (0x8000 for HDD)
    dw  0x0000 ; OFF: reserved tracks (offset)
    db  0x02   ; PSH: log2(sector_size / 128); sector_size = 512
    db  0x03   ; PHM: (sector_size / 128) - 1
valid_dpb_end:
DPB_SIZE equ (valid_dpb_end - valid_dpb)
;
fidems:
; DE - FID environment
; C  - country code
; returns:
; carry flag - true: OK, false: ERROR
; HL         - sing-on/error message
; can corrupt:
; A, BC, DE, IX, IY
; all other registers preserved
    di
    ld de, SECTOR_SIZE
    call SVC_ALLOCATE ; corrupts flags, A, BC, DE; returns in carry flag, HL
    jr nc, _fidems_failed_to_allocate
    push hl
    pop ix
    in a, (RXREG) ; clear ZXMMC's UART RX register
    call zxmmc_getcid_poweron_di ; corrupts flags; returns in A
    cp 0
    jr nz, _fidems_failed
    call idedos_find_partition_table_di ; corrupts A
                                        ; returns in zero flag, HL:DE
    jr nz, _fidems_failed
    push de
    push hl
    push ix
    ld iy, partition_1
    call idedos_find_partition_by_name_di ; corrupts flags, A, BC, DE
                                          ; returns in zero flag
    jr nz, _fidems_find_partition_2
    ld b, DRIVE_C
    call fid_register_drive ; corrupts flags, A, BC, DE, HL, IY
                            ; returns in carry flag, B, IX
    jr nc, _fidems_find_partition_2
    ld a, b
    cp DRIVE_C
    jr nz, _fidems_finished ; something went wrong with drive assignment, exit
_fidems_find_partition_2:
    pop ix
    pop hl
    pop de
    push de
    push hl
    push ix
    ld iy, partition_2
    call idedos_find_partition_by_name_di ; corrupts flags, A, BC, DE
                                          ; returns in zero flag
    jr nz, _fidems_find_partition_3
    ld b, DRIVE_D
    call fid_register_drive ; corrupts flags, A, BC, DE, HL, IY
                            ; returns in carry flag, B, IX
    jr nc, _fidems_find_partition_3
    ld a, b
    cp DRIVE_D
    jr nz, _fidems_finished ; something went wrong with drive assignment, exit
_fidems_find_partition_3:
    pop ix
    pop hl
    pop de
    push de
    push hl
    push ix
    ld iy, partition_3
    call idedos_find_partition_by_name_di ; corrupts flags, A, BC, DE
                                          ; returns in zero flag
    jr nz, _fidems_finished
    ld b, DRIVE_E
    call fid_register_drive ; corrupts flags, A, BC, DE, HL, IY
                            ; returns in carry flag, B, IX
_fidems_finished:
    pop ix
    pop hl
    pop de
    push ix
    pop hl
    ld de, SECTOR_SIZE
    call SVC_DEALLOCATE ; corrupts flags, A, BC, DE, HL; returns nothing
    ld hl, msg_zxmmc
    xor a
    scf ; set CARRY bit for OK
    ei
    ret
_fidems_failed:
    push ix
    pop hl
    ld de, SECTOR_SIZE
    call SVC_DEALLOCATE ; corrupts flags, A, BC, DE, HL; returns nothing
_fidems_failed_to_allocate:
    ld hl, msg_nozxmmc
    or a ; clear CARRY bit for error
    ei
    ret
;
fiddlogon:
; B  - drive
; IX - DPB address
; returns:
; carry flag - true: OK, false: ERROR
; A          - CP/M return code
; can corrupt:
; BC, DE, HL, IX, IY
; all other registers preserved
    ld hl, valid_dpb
    push ix
    pop de
    ld bc, DPB_SIZE
    ldir
    xor a
    scf
    ret
;
fiddread:
; B  - drive
; DE - logical sector
; HL - logical track
; IX - DPB address
; IY - destination
; returns:
; carry flag - true: OK, false: ERROR
; A          - CP/M return code
; can corrupt:
; BC, DE, HL, IX, IY
; all other registers preserved
    push iy
    pop ix ; we won't verify/use DPB
    ; now destination is in IX
    di
    call find_sector ; corrupts flags, A, BC, IY; returns in zero flag, HL:DE
    jr nz, _fid_read_error
    call zxmmc_read_data_di ; corrupts flags; returns in A
    cp 0
    jr nz, _fid_read_error
    call SVC_CATCHUP ; corrupts flags, A, BC, DE, HL; returns nothing
    xor a
    scf
    ei
    ret
_fid_read_error:
    call SVC_CATCHUP ; corrupts flags, A, BC, DE, HL; returns nothing
    or a ; clear CARRY bit for error
    ld a, 0x1 ; CP/M code for non-recoverable error
    ei
    ret
;
fiddwrite:
; B  - drive
; DE - logical sector
; HL - logical track
; IX - DPB address
; IY - source
; returns:
; carry flag - true: OK, false: ERROR
; A          - CP/M return code
; can corrupt:
; BC, DE, HL, IX, IY
; all other registers preserved
    push iy
    pop ix ; we won't verify/use DPB
    di
    call find_sector ; corrupts flags, A, BC, IY; returns in zero flag, HL:DE
    jr nz, _fid_write_error
    call zxmmc_write_data_di ; corrupts flags; returns in A
    cp 0
    jr nz, _fid_write_error
    call SVC_CATCHUP ; corrupts flags, A, BC, DE, HL; returns nothing
    xor a
    scf
    ei
    ret
_fid_write_error:
    call SVC_CATCHUP ; corrupts flags, A, BC, DE, HL; returns nothing
    or a ; clear CARRY bit for error
    ld a, 0x1 ; CP/M code for non-recoverable error
    ei
    ret
;
fiddflush:
; B  - drive
; IX - DPB address
; returns:
; carry flag - true: OK, false: ERROR
; A          - CP/M return code
; can corrupt:
; BC, DE, HL, IX, IY
; all other registers preserved
    xor a
    scf ; set CARRY bit for OK
    ret
;
fiddmess:
; B  - error code
; returns:
; carry flag - true: OK, false: ERROR
; can corrupt:
; other flags
; all other registers preserved
    or a ; clear CARRY bit for error
    ret
;
fid_register_drive:
; B  - drive
; returns:
; carry flag - true: OK, false: ERROR
; B  - drive
; IX - DPB address
; corrupts:
; flags, A, BC, DE, HL, IY
    ld de, jumpblock
    ld hl, 510 ; allocation vector size ((max_blocks + 3) >> 2)
    ld ix, 0 ; directory checksum vector size (0 for mass storage)
    ld iy, 2048 ; hash table size (4 * max. number of directory entries)
    call SVC_D_HOOK
    ret
;
crc16_compute_for_sector:
; IX - sector data address
; returns:
; DE - crc16
; corrupts:
; flags, A, BC
    push hl
    push ix
    ld de, 0
    ld bc, 2 ; 2 * 256 bytes
_crc16_compute_loop:
    ld a, d
    xor (ix + 0)
    ld d, a
    ld l, 8
_crc16_compute_inner:
    ex de, hl
    add hl, hl ; 16-bit shift left 1
    jr nc, _crc16_compute_inner_carry_on
    ld a, h
    xor 0x10
    ld h, a
    ld a, l
    xor 0x21
    ld l, a
_crc16_compute_inner_carry_on:
    ex de, hl
    dec l
    jr nz, _crc16_compute_inner
    inc ix
    djnz _crc16_compute_loop
    dec c
    jr nz, _crc16_compute_loop
    pop ix
    pop hl
    ret
;
chs_to_sector:
; HL - cylinder
; D  - head
; E  - sector
; returns:
; HL:DE - 32-bit sector number (MSB .. LSB)
; corrupts:
; flags, A, BC
    ld a, d
    and 1 ; assuming two heads
    jr z, _chs_to_sector_carry_on
    ld a, e
    or 0x80 ; with the assumption that sector number is always below 128
    ld e, a
_chs_to_sector_carry_on:
    push hl
    pop bc
    ld h, 0
    ld l, b
    ld d, c
    ret
;
find_sector: ; MUST preserve IX
; B - drive
; DE - logical sector
; HL - logical track
; returns:
; zero flag - true: OK, false: ERROR
; HL:DE - 32-bit sector number (MSB .. LSB)
; corrupts:
; flags, A, BC, IY
    ; ensure up to 128 sectors per logical track
    ld a, d
    cp 0
    ret nz
    ld a, e
    and 0x80
    ret nz
    ; ensure up to 256 logical tracks (128 cylinders * 2 tracks, 16MB)
    ld a, h
    cp 0
    ret nz
    ; find current drive data
    ld a, b
    sub DRIVE_C
    ld b, 0
    ld c, a
    sla c
    sla c
    sla c
    sla c
    ld iy, partition_1
    add iy, bc ; address of the current drive data
    ld b, 0
    ld c, (iy + 2) ; consider start head (one track per head)
    add hl, bc ; add partition start head to tracks
    ; convert tracks to cylinders, keep remainder in the lowest bit of D (now 0)
    srl l ; tracks to cylinders (one cylinder is two tracks)
    jr nc, _find_sector_carry_on
    ld d, 1 ; keep remainder
_find_sector_carry_on:
    ld a, h ; H might have lowest bit set after previous add, don't lose it!
    and 1
    jr z, _find_sector_carry_on_further
    ld a, l
    or 0x80
    ld l, a
_find_sector_carry_on_further:
    ; lowest bit of H taken care of, H can be zeroed now
    ld h, 0
    ; add start cylinder to HL
    ld b, (iy + 1)
    ld c, (iy + 0)
    add hl, bc
    jr nc, _find_sector_carry_on_even_further
    or 1 ; overflow! this is bad, reset zero flag and return
    ret
_find_sector_carry_on_even_further:
    ; by now HL should have valid cylinder, D head, E sector
    call chs_to_sector ; corrupts flags, A, BC; returns in HL:DE
    xor a ; set zero flag
    ret
;
memcompare:
; HL - addr1
; DE - addr2
; BC - how many bytes
; returns:
; zero flag - true: matches; false: differs
; corrupts:
; flags, A, BC, DE, HL
    ld a, (de)
    cpi
    ret nz
    ret po
    inc de
    jr memcompare
;
dpb_validate:
; IX - partition entry address
; returns:
; zero flag - true: dpb match; false: no match
; corrupts:
; flags, A
    push bc
    push de
    push hl
    push ix
    pop hl
    ld bc, IDEDOS_PE_XDPB_OFFSET
    add hl, bc
    ld de, valid_dpb
    ld bc, DPB_SIZE
    call memcompare ; corrupts flags, A, BC, DE, HL; returns in zero flag
    pop hl
    pop de
    pop bc
    ret
;
zxmmc_cs_high_di:
    push af
    ld a, 0xff
    out (OUT_PORT), a
    pop af
    ret
;
zxmmc_cs_low_di:
    push af
    ld a, MMC_0
    out (OUT_PORT), a
    pop af
    ret
;
zxmmc_pause_loop_di:
; corrupts:
; flags, A
    push hl
    ld hl, 0x8000
_pause_loop:
    dec hl
    ld a, h
    or l
    jr nz, _pause_loop
    pop hl
    ret
;
zxmmc_clock32_di:
; corrupts:
; flags, A
    push bc
    ld b, 4
_clock32:
    in a, (SPI_PORT)
    djnz _clock32
    pop bc
    ret
;
zxmmc_write_command_di:
; A - command to send
; corrupts:
; flags, A
    push bc
    out (SPI_PORT), a
    ld b, 4
    xor a
_write_cmd_loop:
    out (SPI_PORT), a
    djnz _write_cmd_loop
    ld a, 0x95
    nop
    out (SPI_PORT), a
    pop bc
    ret
;
zxmmc_wait_response_di:
; returns:
; A - response code, 0xff: no response
; corrupts:
; flags
    push bc
    ld bc, 50
_loop_response:
    in a, (SPI_PORT)
    cp 0xff
    jr nz, _response_ok
    djnz _loop_response
    dec c
    jr nz, _loop_response
_response_ok:
    pop bc
    ret
;
zxmmc_send_command_di:
; A     - MMC command
; HL:DE - 32-bit parameter (MSB .. LSB)
; sends 0xff fake checksum
; returns:
; A - 0: no error, otherwise: MMC error code
; corrupts:
; flags
    push bc
    ld c, a
    call zxmmc_cs_high_di ; corrupts nothing; returns nothing
    call zxmmc_clock32_di ; corrupts flags, A; returns nothing
    nop
    call zxmmc_cs_low_di ; corrupts nothing; returns nothing
    ld a, c
    out (SPI_PORT), a
    ld a, h
    nop
    out (SPI_PORT), a
    ld a, l
    nop
    out (SPI_PORT), a
    ld a, d
    nop
    out (SPI_PORT), a
    ld a, e
    nop
    out (SPI_PORT), a
    ld a, 0xff
    nop
    out (SPI_PORT), a
    call zxmmc_wait_response_di ; corrupts flags; returns in A
    cp 0
    jr nz, _mmc_command_error
    pop bc
    ret
_mmc_command_error:
    push af
    call zxmmc_cs_high_di ; corrupts nothing; returns nothing
    in a, (SPI_PORT)
    pop af
    pop bc
    ret
;
zxmmc_waitdata_token_di:
; returns:
; A - code read from ZXMMC
; corrupts:
; flags
    push bc
    ld b, 10
_waitdata_token_loop:
    call zxmmc_wait_response_di ; corrupts flags; returns in A
    cp 0xfe
    jr z, _waitdata_token_exit
    cp 0xff
    jr nz, _waitdata_token_exit
    djnz _waitdata_token_loop
_waitdata_token_exit:
    pop bc
    ret
;
zxmmc_get_cid_di:
; returns:
; A - 0: no error, 1: read error, 2: timeout
; corrupts:
; flags
    push bc
    push de
    push hl
    ld hl, 0
    ld de, 0
    ld a, MMC_READ_CID
    call zxmmc_send_command_di ; corrupts flags; returns in A
    pop hl
    cp 0
    jr z, _get_cid_ok
    push af
    jr _cid_exit
_get_cid_ok:
    call zxmmc_waitdata_token_di ; corrupts flags; returns in A
    cp 0xfe
    jr z, _waitdata_cid_ok
    ld a, 2
    push af
    jr _cid_exit
_waitdata_cid_ok:
    ld b, 18 ; 16 bytes + CRC16
    ld c, SPI_PORT
    inir
    xor a
    push af
_cid_exit:
    call zxmmc_cs_high_di ; corrupts nothing; returns nothing
    in a, (SPI_PORT)
    pop af
    pop de
    pop bc
    ret
;
zxmmc_send_blocksize_di:
; DE - block size in bytes
; corrupts:
; flags, A
    call zxmmc_cs_low_di ; corrupts nothing; returns nothing
    ld a, MMC_SET_BLOCK_SIZE
    out (SPI_PORT), a
    xor a
    nop
    out (SPI_PORT), a
    nop
    nop
    out (SPI_PORT), a
    ld a, d
    nop
    out (SPI_PORT), a
    ld a, e
    nop
    out (SPI_PORT), a
    ld a, 0xff
    nop
    out (SPI_PORT), a
    nop
    nop
    in a, (SPI_PORT)
    nop
    nop
    in a, (SPI_PORT)
    nop
    nop
    call zxmmc_cs_high_di ; corrupts nothing; returns nothing
    ret
;
zxmmc_init_di:
; returns:
; A - 0: no error, 1: reset error, 2: init error
; corrupts:
; flags
    push bc
    call zxmmc_cs_high_di ; corrupts nothing; returns nothing
    ld b, 10
    ld a, 0xff
_init_loop:
    out (SPI_PORT), a
    djnz _init_loop
    nop
    call zxmmc_cs_low_di ; corrupts nothing; returns nothing
    ld a, MMC_GO_IDLE_STATE
    call zxmmc_write_command_di ; corrupts flags, A; returns nothing
    call zxmmc_wait_response_di ; corrupts flags; returns in A
    cp 1
    jr nz, _mmc_reset_failed
    ld bc, 120
_mmc_reset_ok:
    call zxmmc_cs_high_di ; corrupts nothing; returns nothing
    ld a, 0xff
    out (SPI_PORT), a
    nop
    nop
    call zxmmc_cs_low_di ; corrupts nothing; returns nothing
    ld a, MMC_SEND_OP_COND
    call zxmmc_write_command_di ; corrupts flags, A; returns nothing
    call zxmmc_wait_response_di ; corrupts flags; returns in A
    bit 0, a
    jr z, _mmc_init_ok
    djnz _mmc_reset_ok
    dec c
    jr nz, _mmc_reset_ok
    ld a, 2
    jr _mmc_error
_mmc_init_ok:
    call zxmmc_cs_high_di ; corrupts nothing; returns nothing
    in a, (SPI_PORT)
    call zxmmc_pause_loop_di ; corrupts flags, A; returns nothing
    xor a
    pop bc
    ret
_mmc_reset_failed:
    ld a, 1
_mmc_error:
    call zxmmc_cs_high_di ; corrupts nothing; returns nothing
    pop bc
    ret
;
zxmmc_getcid_poweron_di:
; returns:
; A - 0: no error, error otherwise
; corrupts:
; flags
    call zxmmc_get_cid_di ; corrupts flags; returns in A
    cp 0
    ret z
    cp 0xff
    jr z, _poweron_needs_init
    bit 0, a
    jr nz, _poweron_needs_init
    ld a, 1
    ret
_poweron_needs_init:
    call zxmmc_init_di ; corrupts flags; returns in A
    cp 0
    ret nz
    push de
    ld de, SECTOR_SIZE
    call zxmmc_send_blocksize_di ; corrupts flags, A; returns nothing
    pop de
    jr zxmmc_getcid_poweron_di
;
zxmmc_read_data_di:
; HL:DE - 32-bit sector number (MSB .. LSB)
; IX    - destination
; returns:
; A - 0: no error, 1: read error, 2: timeout
; corrupts:
; flags
    ld a, MMC_READ_SINGLE_BLOCK
    call zxmmc_send_command_di ; corrupts flags; returns in A
    cp 0
    ret nz
    call zxmmc_waitdata_token_di ; corrupts flags; returns in A
    cp 0xfe
    jr z, _read_mmc
    ld a, 2
    ret
_read_mmc:
    push hl
    push de
    push ix
    pop hl
    push bc
    ld bc, SPI_PORT
    inir
    nop
    inir
    nop
    nop
    in a, (SPI_PORT)
    ld h, a
    nop
    nop
    in a, (SPI_PORT)
    ld l, a
    call zxmmc_cs_high_di ; corrupts nothing; returns nothing
    call zxmmc_clock32_di ; corrupts flags, A; returns nothing
    ; FUSE sends back 0 here, the real thing sends CRC16 (kept in HL now)
    ; let's make it compatible with both
    ld a, h
    or l
    jr z, _read_crc_ok
    call crc16_compute_for_sector ; corrupts flags, A, BC; returns in DE
    ld a, h
    cp d
    jr nz, _read_crc_error
    ld a, l
    cp e
    jr z, _read_crc_ok
_read_crc_error:
    ld a, 1
    jr _read_done
_read_crc_ok:
    xor a
_read_done:
    pop bc
    pop de
    pop hl
    ret
;
zxmmc_write_data_di:
; HL:DE - 32-bit sector number (MSB .. LSB)
; IX    - source
; returns:
; A - 0: no error, 1: write error, 2: timeout
; corrupts:
; flags
    ld a, MMC_WRITE_SINGLE_BLOCK
    call zxmmc_send_command_di ; corrupts flags; returns in A
    cp 0
    ret nz
    push hl
    push bc
    ld a, 0xfe
    out (SPI_PORT), a ; send data token
    nop
    nop
    ld bc, SPI_PORT
    push ix
    pop hl
    otir
    nop
    otir
    nop
    nop
    ld a, 0x95
    out (SPI_PORT), a
    nop
    nop
    out (SPI_PORT), a
    call zxmmc_wait_response_di ; corrupts flags; returns in A
    pop bc
    pop hl
    and 0x1f
    cp 5
    jr nz, _write_mmc_error
_write_mmc_wait_busy:
    call zxmmc_wait_response_di ; corrupts flags; returns in A
    cp 0
    jr z, _write_mmc_wait_busy
    call zxmmc_cs_high_di ; corrupts nothing; returns nothing
    call zxmmc_clock32_di ; corrupts flags, A; returns nothing
    xor a
    ret
_write_mmc_error:
    ld a, 2
    ret
;
idedos_found_partition_table:
; IX - buffer to examine
; returns:
; zero flag - true: partition table id match; false: partition table not found
; corrupts:
; flags, A
    ld a, (ix + 0)
    cp 'P'
    ret nz
    ld a, (ix + 1)
    cp 'L'
    ret nz
    ld a, (ix + 2)
    cp 'U'
    ret nz
    ld a, (ix + IDEDOS_PE_TYPE)
    cp IDEDOS_TYPE_PARTITION_TBL
    ret nz
    ld a, (ix + IDEDOS_PE_START_CYL_LO)
    cp 0 ; ensure start at cylinder zero (LSB)
    ret nz
    ld a, (ix + IDEDOS_PE_START_CYL_HI)
    cp 0 ; ensure start at cylinder zero (MSB)
    ret nz
    ld a, (ix + IDEDOS_PT_NUM_HEADS)
    cp 2 ; ensure two heads (ZXMMC specific)
    ret nz
    ld a, (ix + IDEDOS_PT_SECT_PER_TRACK)
    cp 128 ; ensure 128 sectors per track (ZXMMC specific)
    ret nz
    ld a, (ix + IDEDOS_PT_SECT_PER_CYL_LO)
    cp 0 ; ensure 256 sectors per cylinder (ZXMMC specific) (LSB)
    ret nz
    ld a, (ix + IDEDOS_PT_SECT_PER_CYL_HI)
    cp 1 ; ensure 256 sectors per cylinder (ZXMMC specific) (MSB)
    ret
;
idedos_find_partition_table_di:
; IX - read buffer
; returns:
; zero flag - true: found, false: not found
; HL:DE - IDEDOS partition table start sector (MSB .. LSB)
; corrupts:
; flags, A
    ld hl, 0
    ld de, 0
    call zxmmc_read_data_di ; corrupts flags; returns in A
    cp 0
    ret nz
    call idedos_found_partition_table ; corrupts flags, A; returns in zero flag
    ret z
    ld de, 128
    call zxmmc_read_data_di ; corrupts flags; returns in A
    cp 0
    ret nz
    call idedos_found_partition_table ; corrupts flags, A; returns in zero flag
    ret
;
idedos_examine_named_partition:
; IX - partition to examine
; IY - partition name looked for (overwritten by C/H/S if partition is found!)
; returns:
; zero flag - true: positive match, false: no match
; IX - next partition to examine
; corrupts:
; flags, A
    push bc
    push de
    push hl
    ld a, (ix + IDEDOS_PE_TYPE)
    cp IDEDOS_TYPE_PLUS3DOS
    jr nz, _idedos_partition_exam_finished
    push ix
    pop hl
    push iy
    pop de
    ld bc, 16
    call memcompare ; corrupts flags, A, BC, DE, HL; returns in zero flag
    jr nz, _idedos_partition_exam_finished
    ; ensure 16MB partition size
    ld a, (ix + IDEDOS_PE_START_HEAD)
    or 1
    cp 1
    jr nz, _idedos_partition_exam_finished
    ld a, (ix + IDEDOS_PE_START_HEAD)
    xor (ix + IDEDOS_PE_END_HEAD)
    cp 1
    jr nz, _idedos_partition_exam_finished
    ld h, (ix + IDEDOS_PE_START_CYL_HI)
    ld l, (ix + IDEDOS_PE_START_CYL_LO)
    ld bc, ZXMMC_CYLS_PER_16MB_PARTITION_MINUS_ONE
    add hl, bc
    ld b, 0
    ld c, (ix + IDEDOS_PE_START_HEAD)
    add hl, bc
    ld a, h
    cp (ix + IDEDOS_PE_END_CYL_HI)
    jr nz, _idedos_partition_exam_finished
    ld a, l
    cp (ix + IDEDOS_PE_END_CYL_LO)
    jr nz, _idedos_partition_exam_finished
    call dpb_validate ; corrupts flags, A; returns in zero flag
    jr nz, _idedos_partition_exam_finished
    ld a, (ix + IDEDOS_PE_START_CYL_LO)
    ld (iy + 0), a
    ld a, (ix + IDEDOS_PE_START_CYL_HI)
    ld (iy + 1), a
    ld a, (ix + IDEDOS_PE_START_HEAD)
    ld (iy + 2), a
_idedos_partition_exam_finished:
    push af
    ld bc, IDEDOS_PE_SIZE
    add ix, bc
    pop af
    pop hl
    pop de
    pop bc
    ret
;
idedos_find_partition_by_name_di:
; HL:DE - 32-bit current sector number (MSB .. LSB)
; IX    - read buffer
; IY    - partition name (will be overwritten by C/H/S if partition is found!)
; returns:
; zero flag - true: found, false: not found
; corrupts:
; flags, A, BC, DE
    push ix
    call zxmmc_read_data_di ; corrupts flags; returns in A
    cp 0
    jr nz, _idedos_find_partition_done
    ld b, IDEDOS_PARTITIONS_PER_SECTOR
_idedos_find_partition_loop:
    ld a, (ix + IDEDOS_PE_TYPE)
    cp IDEDOS_TYPE_UNUSED
    jr z, _idedos_partition_unused_encountered
    call idedos_examine_named_partition ; corrupts flags, A
                                        ; returns in zero flag and IX
    jr z, _idedos_find_partition_done
    djnz _idedos_find_partition_loop
    inc de
    pop ix
    jr idedos_find_partition_by_name_di ; can fall into infinite loop here!
_idedos_partition_unused_encountered:
    or 1 ; reset zero flag
_idedos_find_partition_done:
    pop ix
    ret
;
    end
