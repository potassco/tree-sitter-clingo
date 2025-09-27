; TODO: support for aligning aggregate elements would be a nice idea as well.

((statement) @indent.align
  (#contains? @indent.align ":-")
  (#set! indent.open_delimiter ":-")
  (#set! indent.close_delimiter ".")
  (#set! indent.increment 3))

((ERROR) @indent.align
  (#contains? @indent.align ":-")
  (#set! indent.open_delimiter ":-")
  (#set! indent.close_delimiter ".")
  (#set! indent.increment 3))

((statement) @indent.align
  (#not-contains? @indent.align ":-")
  (#set! indent.open_delimiter ":")
  (#set! indent.close_delimiter ".")
  (#set! indent.increment 2))

((ERROR) @indent.align
  (#not-contains? @indent.align ":-")
  (#set! indent.open_delimiter ":")
  (#set! indent.close_delimiter ".")
  (#set! indent.increment 2))
