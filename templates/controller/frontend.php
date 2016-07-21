<?php

class {{className}} extends {{parentClassName}} {
    public function indexAction() {
        $this->loadLayout()->renderLayout();
    }

    public function jsonAction() {
        $data = json_encode(array('foo' => 'bar'));
        $this->getResponse()->setHeader('Content-type', 'application/json; charset=utf-8');
        $this->getResponse()->setBody($data);
    }

    public function htmlAction() {
        $html = $this->getLayout()
            ->createBlock('modulename/foo')
            ->setTemplate('modulename/foo.phtml')
            ->toHtml();

        $this->getResponse()->setHeader('Content-type', 'text/html; charset=utf-8');
        $this->getResponse()->setBody($html);
    }
}
