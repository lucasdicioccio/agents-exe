(function(){
  const addDialog = (content, f) => {
    let pre = document.createElement('pre');
    pre.style = 'max-height: 35vh; overflow: scroll;';
    pre.appendChild(document.createTextNode(content));

    let btn = document.createElement('button');
    btn.className = "dev-produce-button";
    btn.appendChild(document.createTextNode("close"));

    let div = document.createElement('div');
    div.style = 'position:fixed; inset:20%; background-color:white; padding: 1em; border: 1px solid black';
    div.appendChild(pre);
    div.appendChild(btn);

    var navDom = document.getElementById('site-navigation');
    btn.onclick = () => div.remove();
    navDom.appendChild(div);
  };
  const callRoute = (path) => (ev) => {
    const btn = ev.target;
    btn.disabled = true;
    const url = new URL(document.location.origin + path);
    fetch(url, {'method': 'POST'})
    .then(response => response.json())
    .then(data => {
      addDialog(data);
      btn.disabled = false;
    })
    .catch(error => {
      console.error('Error:', error);
      btn.disabled = false;
      alert("failed");
    })
  };
  const addButton = (name, f) => {
    let btn = document.createElement('button');
    btn.className = "dev-produce-button";
    btn.onclick = f;
    btn.appendChild(document.createTextNode(name));
    var navDom = document.getElementById('site-navigation');
    navDom.appendChild(btn);
  };
  const go = () => {
    addButton("produce", callRoute("/dev/produce"));
    addButton("reload", callRoute("/dev/reload"));

    const url = new URL(document.location.origin + "/dev/commands");
    fetch(url, {'method': 'GET'})
    .then(response => response.json())
    .then(data => {
      data.forEach((cmd) => addButton(cmd.display, callRoute(`/dev/command?handle=${cmd.handle}`)));
      console.log(data);
    })
    .catch(error => {
      console.error('Error:', error);
    })
  };
  
  document.addEventListener("DOMContentLoaded", go);
})();
