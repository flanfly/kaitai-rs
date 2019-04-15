meta:
  id: websocket
  title: WebSocket
  xref:
    rfc: 6455
  endian: be
  license: CC0-1.0
doc: |
  The WebSocket protocol establishes a two-way communication channel via TCP.
  Messages are made up of one or more dataframes, and are delineated by
  frames with the `fin` bit set.
seq:
  - id: dataframes
    type: dataframe
    repeat: until
    repeat-until: _.finished

types:
  dataframe:
    seq:
      - id: finished
        -orig-id: fin
        type: b1
      - id: reserved
        -orig-id: 'rsv1, rsv2, rsv3'
        type: b3
      - id: opcode
        enum: opcode
        type: b4
      - id: is_masked
        type: b1
      - id: len_payload_primary
        type: b7
      - id: len_payload_extended_1
        type: u2
        if: len_payload_primary == 126
      - id: len_payload_extended_2
        type: u4
        if: len_payload_primary == 127
      - id: mask_key
        type: u4
        if: is_masked
      - id: payload_bytes
        size: len_payload
        if: '_root.dataframes[0].opcode != opcode::text'
      - id: payload_text
        size: len_payload
        type: str
        encoding: UTF-8
        if: '_root.dataframes[0].opcode == opcode::text'
    
    instances:
      len_payload:
        value: |
          len_payload_primary <= 125 ? len_payload_primary : (
            len_payload_primary == 126 ? len_payload_extended_1 : len_payload_extended_2
          )
    
enums:
  opcode:
    0: continuation
    1: text
    2: binary
    3: reserved_3
    4: reserved_4
    5: reserved_5
    6: reserved_6
    7: reserved_7
    8: close
    9: ping
    0xA: pong
    0xB: reserved_control_b
    0xC: reserved_control_c
    0xD: reserved_control_d
    0xE: reserved_control_e
    0xF: reserved_control_f
