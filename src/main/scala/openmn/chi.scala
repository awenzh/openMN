package openmn

import spinal.core._
import java.util.ResourceBundle
import _root_.javax.swing.plaf.SpinnerUI
import scala.collection.immutable.IntMap








case class ChiParam ( qoS_Width : Int = 4,
                         nodeID_Width : Int = 6,    //  max 64
                         txnID_Width : Int = 6,     //  max 64
                         dbID_Width : Int = 6,      //  max 64
                         opcode_Width : Int = 4,
                         size_Width : Int = 4,      //  1/2/4/8/16/32/64/128/512 byte
                         addr_Width : Int = 40,
                         memAttr_Width : Int = 4,
                         resp_Width : Int = 2,
                         dataID_Width : Int = 4,
                         data_Width : Int = 512,
                         be_Width : Int = data_Width/8,
                         poison_Width : Int = data_Width/64,
                         rsvdc_Width : Int = 16,
                         dualW_Chnl : Boolean = false ){

  val reqChnl_Width = qoS_Width + nodeID_Width + nodeID_Width + txnID_Width + opcode_Width + size_Width + addr_Width + memAttr_Width + rsvdc_Width
  val rspChnl_Width = qoS_Width + nodeID_Width + nodeID_Width + txnID_Width + opcode_Width + resp_Width + dbID_Width + rsvdc_Width
  val datChnl_Width = qoS_Width + nodeID_Width + nodeID_Width + txnID_Width + opcode_Width + resp_Width + dbID_Width + dataID_Width + data_Width + be_Width + poison_Width + rsvdc_Width

  val qos_LHS = 0
  val tgtID_LHS = qoS_Width
  val srcID_LHS = tgtID_LHS + nodeID_Width
  val txnID_LHS = srcID_LHS + nodeID_Width
  val opcode_LHS= txnID_LHS + txnID_Width

  val size_LHS  = opcode_LHS + opcode_Width
  val addr_LHS  = size_LHS + size_Width
  val memAttr_LHS = addr_LHS + addr_Width

  val resp_LHS  = opcode_LHS + opcode_Width
  val dbID_LHS  = resp_LHS + resp_Width
  val dataID_LHS= dbID_LHS + dbID_Width
  val data_LHS  = dataID_LHS + dataID_Width
  val be_LHS    = dataID_LHS + data_Width
  val poison_LHS= be_LHS + be_Width
}

case class ChiFlit(chnl:String = "REQ", config:ChiParam) extends Bundle {
  val FLIT = chnl match {
    case "REQ" => Bits(config.reqChnl_Width bits)
    case "RSP" => Bits(config.rspChnl_Width bits)
    case "DAT" => Bits(config.datChnl_Width bits)
    case _     => SpinalError(s"Only suuport REQ/RSP/DAT."); null
  }
  def qos     = FLIT(config.qos_LHS, config.qoS_Width bits)
  def tgtid   = FLIT(config.tgtID_LHS, config.nodeID_Width bits)
  def srcid   = FLIT(config.srcID_LHS, config.nodeID_Width bits)
  def txnid   = FLIT(config.txnID_LHS, config.txnID_Width bits)
  def opcode  = FLIT(config.opcode_LHS, config.opcode_Width bits)
  def rsvdc   = FLIT.resizeLeft(config.rsvdc_Width)
  def size:Bits = {
    chnl match {
      case "REQ" => FLIT(config.size_LHS, config.size_Width bits) 
      case _     => FLIT(0, config.size_Width bits)
    }
  }
  def addr:Bits = {
    chnl match {
      case "REQ" => FLIT(config.addr_LHS, config.addr_Width bits)
      case _     => FLIT(0, config.addr_Width bits)
    }
  }
  def memattr:Bits = {
    chnl match {
      case "REQ" => FLIT(config.memAttr_LHS, config.memAttr_Width bits)
      case _     => FLIT(0, config.memAttr_Width bits)
    }
  }
  def resp    = FLIT(config.resp_LHS, config.reqChnl_Width bits)
  def dbid    = FLIT(config.dbID_LHS, config.dbID_Width bits)
  def dataid  = FLIT(config.dataID_LHS, config.dataID_Width bits)
  def dat0:Bits = {
    chnl match {
      case "DAT" => FLIT(config.data_LHS, config.data_Width bits)
      case _     => Bits(0).resize(config.dataID_Width)
    }
  }
  def be:Bits = {
    chnl match {
      case "DAT" => FLIT(config.be_LHS, config.be_Width bits)
      case _     => Bits(0).resize(config.be_Width)
    }
  }
  def poison:Bits = {
    chnl match {
      case "DAT" => FLIT(config.poison_LHS, config.poison_Width bits)
      case _     => Bits(0).resize(config.poison_Width)
    }
  }
}

case class ChiChnl(chnl:String = "REQ", config:ChiParam) extends Bundle with IMasterSlave{
  val FLITPEND = Bool()
  val FLITV    = Bool()
  val FLIT     = ChiFlit(chnl, config)
  val LCRDV    = Bool()
  override def asMaster():Unit = {
    out(FLITPEND, FLITV, FLIT)
    in(LCRDV)
  }
}

case class ItfChnl(itf:String = "rout", config:ChiParam) extends Bundle with IMasterSlave {
  val REQ = ChiChnl("REQ", config)
  val RSP = ChiChnl("RSP", config)
  val DAT0= ChiChnl("DAT", config)
  val DAT1= if (config.dualW_Chnl) ChiChnl("DAT", config) else null
  override def asMaster():Unit = {
    master(REQ, RSP, DAT0, DAT1)
  }
}

case class NodeChnl(config:ChiParam) extends Bundle with IMasterSlave {
  val REQ = ChiChnl("REQ", config)
  val RSP = ChiChnl("RSP", config)
  val DAT0= ChiChnl("DAT", config)
  val DAT1= ChiChnl("DAT", config)
  over
}




case class ChnlParam (
  reqWidth:Int = 131,
  rspWidth:Int = 65,
  snpWidth:Int = 95,
  datWidth:Int = 676,
  mulDatRx:Boolean = false,
  mulDatTx:Boolean = false,
)
case class InnerChnl(p:ChnlParam) extends Bundle with IMasterSlave {
  val REQ   = ChnlFlit(p.reqWidth)
  val RSP   = ChnlFlit(p.rspWidth)
  val SNP   = ChnlFlit(p.snpWidth)
  val DAT   = ChnlFlit(p.datWidth)
  val DATE  = if(p.mulDatRx) ChnlFlit else null
  override def asMaster():Unit = {
    master(REQ, RSP, SNP, DAT, DATE)
  }
}
case class RnfChnl(p:ChnlParam) extends Bundle with IMasterSlave {
  val TXREQ = ChnlFlit(p.reqWidth)
  val RXRSP = ChnlFlit(p.rspWidth)
  val RXDAT = ChnlFlit(p.datWidth)
  val RXDATE= if(p.mulDatRx) ChnlFlit(p.datWidth) else null
  val RXSNP = ChnlFlit(p.snpWidth)
  val TXRSP = ChnlFlit(p.rspWidth)
  val TXDAT = ChnlFlit(p.)


  val REQ   = ChnlFlit(p.reqWidth)
  val RSP   = ChnlFlit(p.rspWidth)
  val SNP   = ChnlFlit(p.snpWidth)
  val DAT   = ChnlFlit(p.datWidth)
  val DATE  = if (p.mulDat) ChnlFlit else null
  override def asMaster():Unit = {
    master(REQ, RSP, SNP, DAT, DATE)
  } 


  test
}