import {Elm} from '../src/Main.elm'

window.ShowAvlTree =
  {
    appModule: Elm.Main,
    configurePorts: app => {
      var sendSize = function (element) {
        var width = parseInt(element.getAttribute('width'), 10);
        var height = parseInt(element.getAttribute('height'), 10);
        app.ports.size.send([width, height]);
      };

      app.ports.initSizeInfo.subscribe(function() {
        var MutationObserver = window.MutationObserver || window.WebKitMutationObserver || window.MozMutationObserver;

        var element = document.getElementsByClassName('show-avl-tree-wrapper')[0].children[0];
        sendSize(element);

        var observer = new MutationObserver(function (mutations) {
          sendSize(element);
        });

        observer.observe(element, {
          attributes: true //configure it to listen to attribute changes
        });
      });
    }
  };
