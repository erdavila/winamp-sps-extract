package player.script

trait DSPScript {
  def initialize(ctx: InitializeContext): Unit
  def onSliderChange(ctx: SliderChangeContext): Unit
  def onSample(ctx: SampleContext): Unit
}
