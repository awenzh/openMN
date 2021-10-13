package openmn

import spinal.core._
import java.util.ResourceBundle

object chi{

  TXSACTIVE
  RXSACTIVE
  TXLINKACTIVEREQ
  TXLINKACTIVEACK
}

case class ChnlFlit(w : Int) extends Bundle with IMasterSlave {
  val FLITPEND  = Bool()
  val FLITV     = Bool()
  val FLIT      = Bits(w bits)
  val LCRDV     = Bool()

  override def asMaster():Unit = {
    out(FLITPEND, FLITV, FLIT)
    in(LCRDV)
  }
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