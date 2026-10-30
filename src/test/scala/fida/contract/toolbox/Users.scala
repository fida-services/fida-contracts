package fida.contract.toolbox

import scalus.testing.kit.Party

/** The wallets used by the tests (port of `Fida.Contract.TestToolbox.Users`). */
final case class Users(
    fidaSystem: User,
    broker1: User,
    broker2: User,
    policyHolder: User,
    investor1: User,
    investor2: User,
    investor3: User
) {
    def toSeq: Seq[User] =
        Seq(fidaSystem, broker1, broker2, policyHolder, investor1, investor2, investor3)
}

object Users {
    val all: Users = Users(
      fidaSystem = User("Fida System Wallet", Party.Trent),
      broker1 = User("Policy Broker 1", Party.Bob),
      broker2 = User("Policy Broker 2", Party.Charles),
      policyHolder = User("Policy Holder", Party.Alice),
      investor1 = User("Investor 1", Party.Dave),
      investor2 = User("Investor 2", Party.Eve),
      investor3 = User("Investor 3", Party.Grace)
    )
}
