;; switch the path returns to have expanded file names, actually absolute.

; 

(defun mage-get-absolute-path()
  "Gets the absolute path of the relevant Magento installation."
  (interactive)
  (locate-dominating-file (buffer-file-name) "app/Mage.php"))

(defun mage-get-module-path()
  "Gets the absolute path of the relevant Magento module."
  (interactive)
  (locate-dominating-file (buffer-file-name) "etc/config.xml"))

(defun mage-send-request()
  "Gets the php code necessary to access the magento installation."
  (interactive)
  (format "include('%sapp/Mage.php');
umask(0);
Mage::app()->setCurrentStore(Mage_Core_Model_App::ADMIN_STORE_ID);" (expand-file-name (mage-get-absolute-path))))

(defun mage-base-url()
  "Gets the base URL of the magento installation."
  (interactive)
  (shell-command (format "php -r \"%s echo Mage::getBaseUrl();\"" (mage-send-request))))

(defun mage-module-config-file()
  "Loads the /etc/config.xml of the relevant module the current buffer is in."
  (interactive)
  (unless 'mage-get-module-path nil
	  (find-file (format "%setc/config.xml" (mage-get-module-path)))))

;; Doesn't work atm! Blah!
(defun mage-eval(mageline)
  "Evaluates a line of PHP ran in Magento Installation."
  (interactive "sMagento Eval: ")
  (shell-command (format "php -r \"%s %s\"" (mage-send-request) (mageline))))  

(defun mage-get-selection()
  "Gets the selected text"
  (interactive)
  (buffer-substring (mark) (point)))

(defun mage-file-from-class()
  (interactive)
  (shell-command (format "php -r \"%secho mageFindClassFile('%s');\"" (mage-send-request) (mage-get-selection))))

(defun mage-load-file()
  "Loads the Magento file relative to the highlighted text, automagic go!"
  (interactive)
  ; If it matches Mage::getModel(foo/bar) - mage-get-model-class-name, etc, etc
)

(defun mage-get-helper-class-name(helper-uri)
  "Gets the class name of the helper from the uri.")

(defun mage-get-model-class-name(model-uri)
  "Gets the class name of the model from the uri.")

(defun mage-get-block-class-name(block-uri)
  "Gets the class name of the block from the uri.")