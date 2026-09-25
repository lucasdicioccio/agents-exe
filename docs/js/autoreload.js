(function(){
  const currentURLParams = new URLSearchParams(window.location.search);
  let serverId = currentURLParams.get('server-id');
  const go = (n) => () => {
    const m = n < 5 ? 1+n : 5;
    const retry = () => setTimeout(go(m), n*1000);
    const url = new URL(document.location.origin + '/dev/watch');
    let params = [['pathname', document.location.pathname]];
    if ((serverId ?? '') !== '') {
      params.push(['server-id', serverId]);
    }
    url.search = new URLSearchParams(params).toString();
      console.log(`fetching ${url}`);
    fetch(url)
    .then(response => response.json())
    .then(data => {
      console.log("got response");
      serverId = data[0];
      if (data[1] === "Respawned") {
        const params = new URLSearchParams();
        params.set('server-id', serverId);
        window.location.search = params;
      }
      if (data[1] === "Reloaded") {
        location.reload();
      }
      if (data[1] === "Disappeared") {
        retry();
      }
    })
    .catch(error => {
      console.error('Error:', error);
      retry();
    })
  };
  go(0)();
})();
