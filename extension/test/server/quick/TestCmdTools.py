"""
This file is part of Arakoon, a distributed key-value store. Copyright
(C) 2010 Incubaid BVBA

Licensees holding a valid Incubaid license may use this file in
accordance with Incubaid's Arakoon commercial license agreement. For
more information on how to enter into this agreement, please contact
Incubaid (contact details can be found on www.arakoon.org/licensing).

Alternatively, this file may be redistributed and/or modified under
the terms of the GNU Affero General Public License version 3, as
published by the Free Software Foundation. Under this license, this
file is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.

See the GNU Affero General Public License for more details.
You should have received a copy of the
GNU Affero General Public License along with this program (file "COPYING").
If not, see <http://www.gnu.org/licenses/>.
"""

from pymonkey import q
from nose.tools import *
import subprocess
import os
import time
import logging

class TestCmdTools:
    def __init__(self):
        cid = 'sturdy'
        self._clusterId = cid
        self._n0 = "%s_0" % cid
        self._n1 = "%s_1" % cid
        self._n2 = "%s_2" % cid

    def _serverNodes(self):
        return "%s_servernodes" % self._clusterId

    def _getCluster(self):
        return q.manage.arakoon.getCluster(self._clusterId)
    
    def setup(self):
        logging.info('setup')
        if self._clusterId in q.config.list():
            q.config.remove(self._clusterId)
        
        servernodes = self._serverNodes()
        if servernodes in q.config.list():
            q.config.remove(servernodes)

        cluster = self._getCluster()
        cluster.setUp(3)

    def teardown(self):
        logging.info('teardown')
        cluster = self._getCluster()
        cluster.stop()
        cluster.tearDown()
        cluster.remove()

    def _assert_n_running(self,n):
        cluster = self._getCluster()
        status = cluster.getStatus()
        logging.debug('status=%s', status)
        c = 0
        for key in status.keys():
            if status[key] == q.enumerators.AppStatusType.RUNNING:
                c = c + 1
        assert_equals(c, n)

    def testStart(self):
        cluster = self._getCluster()
        cluster.start()
        self._assert_n_running(3)

        #starting twice should not throw anything
        cluster.start()
        self._assert_n_running(3)



    def testStop(self):
        cluster = self._getCluster()
        cluster.start()
        self._assert_n_running(3)

        cluster.stop()
        self._assert_n_running(0)

        #stopping twice should not throw anything
        cluster.stop()
        self._assert_n_running(0)

    def testRestart(self):
        cluster = self._getCluster()
        cluster.start()
        self._assert_n_running(3)
        logging.info("3 servers running, going to restart")
        cluster.restart()
        #@TODO check if the pids are different
        logging.info("restarted")
        self._assert_n_running(3)
        logging.info("again, 3 servers running")
        cluster.stop()
        logging.info("stopped")
        self._assert_n_running(0)
        logging.info("restart when stopped")
        cluster.restart()
        self._assert_n_running(3)

    def testStartOne(self):
        cluster = self._getCluster()
        cluster.startOne(self._n0)
        self._assert_n_running(1)
        cluster.startOne(self._n1)
        self._assert_n_running(2)

    def testStartOneUnkown(self):
        cluster = self._getCluster()
        assert_raises(Exception, cluster.startOne, "arakoon0")

    def testStopOne(self):
        cluster = self._getCluster()
        cluster.start()
        t0 = time.clock()
        cluster.stopOne(self._n0)
        t1 = time.clock()
        assert_true(t1 - t0 < 2.0, "took too long")
        self._assert_n_running(2)

    def testStopOneUnknown(self):
        cluster = self._getCluster()
        assert_raises(Exception, cluster.stopOne, "arakoon0")

    def testGetStatus(self):
        logging.info('1')
        cluster = self._getCluster()
        logging.info('2')
        cluster.start()
        logging.info('3')
        assert_equals(cluster.getStatus(),
                      {self._n0: q.enumerators.AppStatusType.RUNNING,
                       self._n1: q.enumerators.AppStatusType.RUNNING,
                       self._n2: q.enumerators.AppStatusType.RUNNING})
        logging.info('4')
        cluster.stopOne(self._n0)
        assert_equals(cluster.getStatus(),
                      {self._n0: q.enumerators.AppStatusType.HALTED, 
                       self._n1: q.enumerators.AppStatusType.RUNNING,
                       self._n2: q.enumerators.AppStatusType.RUNNING})
        
    def testGetStatusOne(self):
        cluster = self._getCluster()
        cluster.start()
        cluster.stopOne(self._n0)
        gos = cluster.getStatusOne
        assert_equals(gos(self._n0), q.enumerators.AppStatusType.HALTED)
        assert_equals(gos(self._n1), q.enumerators.AppStatusType.RUNNING)

    def testGetStatusOneUnknown(self):
        cluster = self._getCluster()
        assert_raises(Exception, cluster.getStatusOne, "whatever")

    def testRestartOne(self):
        cluster = self._getCluster()
        cluster.start()
        cluster.stopOne(self._n0)
        cluster.restartOne(self._n1)
        self._assert_n_running(2)


    def testRestartOneUnknown(self):
        cluster = self._getCluster()
        assert_raises(Exception, cluster.restartOne, "arakoon0")

if __name__ == '__main__' :
    from pymonkey import InitBase
    pass
