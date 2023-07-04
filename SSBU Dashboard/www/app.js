async function loadScript() {

  // use fetch to get script
  const response = await fetch('https://files.sundial.nyc/smashultimate/web/main.js');
  const text = await response.text();

  console.log('eval script length', text.length);
  eval(text);

}

loadScript();
