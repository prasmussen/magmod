<?php

class {{className}} extends {{parentClassName}} {
    protected function _isAllowed() {
        // TODO: Add proper resource identifier if acl is used
        // See: http://magento.stackexchange.com/questions/73646/access-denied-errors-after-installing-supee-6285
        // return Mage::getSingleton('admin/session')->isAllowed('ENTER RESOURCE IDENTIFIER HERE');
        return true;
    }

    public function indexAction() {
        $this->loadLayout()->renderLayout();
    }

    public function jsonAction() {
        $data = json_encode(array('foo' => 'bar'));
        $this->getResponse()->setHeader('Content-type', 'application/json');
        $this->getResponse()->setBody($data);
    }

    public function htmlAction() {
        $html = $this->getLayout()
            ->createBlock('modulename/foo')
            ->setTemplate('modulename/foo.phtml')
            ->toHtml();

        $this->getResponse()->setHeader('Content-type', 'text/html');
        $this->getResponse()->setBody($html);
    }
}
