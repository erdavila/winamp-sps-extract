package player.script

case class InitializeContext(
  nch: Int,
  srate: Int,
)

case class SliderChangeContext(
  nch: Int,
  slider1: Double,
  slider2: Double,
  slider3: Double,
  slider4: Double,
  srate: Int,
)

case class SampleContext(
  megabuf: Megabuf,
  nch: Int,
  var repeat: Boolean,
  var skip: Boolean,
  slider1: Double,
  slider2: Double,
  slider3: Double,
  slider4: Double,
  var spl0: Double,
  var spl1: Double,
  srate: Int,
  var trig1: Boolean,
  var trig2: Boolean,
  var trig3: Boolean,
  var trig4: Boolean,
)
