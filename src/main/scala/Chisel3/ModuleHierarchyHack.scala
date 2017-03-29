package chisel3

trait ModuleWithParentInfo extends Module {
  def getParent = _parent
}