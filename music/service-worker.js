var cacheName = "csound-live-code-15";

var filesToCache = [
  "/",
  "/index.html",
  "/service-worker.js",
  "/orcs/Agonoize.orc",
  "/orcs/AsymFM.orc",
  "/orcs/BandlimitedSum.orc",
  "/orcs/BaBass.orc",
  "/orcs/BasicKeys.orc",
  "/orcs/BassDrum.orc",
  "/orcs/Bell.orc",
  "/orcs/Bouffon.orc",
  "/orcs/Breathe.orc",
  "/orcs/ClosedHat.orc",
  "/orcs/Decades.orc",
  "/orcs/Devine.orc",
  "/orcs/Distar.orc",
  "/orcs/Dulcimer.orc",
  "/orcs/Dummy.orc",
  "/orcs/DxKeys.orc",
  "/orcs/Elephant.orc",
  "/orcs/Enough.orc",
  "/orcs/Ensemble.orc",
  "/orcs/FMKeys.orc",
  "/orcs/Fusion.orc",
  "/orcs/GhostPad.orc",
  "/orcs/Grain.orc",
  "/orcs/GranularFormant.orc",
  "/orcs/Guitar.orc",
  "/orcs/HandClap.orc",
  "/orcs/Hocico.orc",
  "/orcs/HsLead.orc",
  "/orcs/Hunter.orc",
  "/orcs/Kick.orc",
  "/orcs/Learn.orc",
  "/orcs/Logan.orc",
  "/orcs/Naive.orc",
  "/orcs/OpenHat.orc",
  "/orcs/Pad1.orc",
  "/orcs/PhaseAlignedFormant.orc",
  "/orcs/PowerHyperKeys.orc",
  "/orcs/PsyBass.orc",
  "/orcs/PsyKick.orc",
  "/orcs/PulseKeys.orc",
  "/orcs/PulseLead.orc",
  "/orcs/Resonance1.orc",
  "/orcs/ResonantPad.orc",
  "/orcs/RiseAndFallPad.orc",
  "/orcs/Sbire.orc",
  "/orcs/Shyish.orc",
  "/orcs/Snare.orc",
  "/orcs/SnareDrum.orc",
  "/orcs/Snobby.orc",
  "/orcs/Spine.orc",
  "/orcs/StringSection.orc",
  "/orcs/Supersaw.orc",
  "/orcs/Tales.orc",
  "/orcs/ThinPad.orc",
  "/orcs/Vampyr.orc",
  "/orcs/Violin.orc",
  "/orcs/VoxFM.orc",
  "/ops/arpmidi_i_iik[].op",
  "/ops/bits_i_k[].op",
  "/ops/casio_rez1_a_ik.op",
  "/ops/chorus_a_aki.op",
  "/ops/hs_sinebuzz_a_kkk.op",
  "/ops/number_of_ones_i_i.op",
  "/ops/number_of_ones_i_S.op",
  "/ops/pm_sine_a_kk.op",
  "/ops/pulse_lead_core_a_KKK.op",
  "/ops/saw_a_kk.op",
  "/ops/score,0,S[]i[]i[]i[]j.op",
  "/ops/tempo_sync_delay_aa_aa.op",
  "/ops/wavesolve_aa_aikk.op",
  "/web/images/icons/icon-144x144.png",
  "/web/codemirror.css",
  "/web/theme/monokai.css",
  "/web/cslivecode.css",
  "/web/codemirror.js",
  "/web/mode/csound/csound.js",
  "/web/addon/comment/comment.js",
  "/web/addon/edit/matchbrackets.js",
  "/web/addon/edit/closebrackets.js",
  "/web/keymap/vim.js",
  "/web/keymap/emacs.js",
  "/web/cslivecode.js",
  "/web/csound/CsoundNode.js",
  "/web/csound/CsoundObj.js",
  "/web/csound/CsoundProcessor.js",
  "/web/csound/CsoundScriptProcessorNode.js",
  "/web/csound/libcsound-worklet.js",
  "/web/csound/libcsound.js",
  "/livecode.orc",
  "/start.orc",
  "https://fonts.googleapis.com/css?family=Raleway|Roboto:400,700",
  "https://use.fontawesome.com/releases/v5.1.1/css/all.css",
  "https://code.jquery.com/jquery-1.11.1.min.js",
  "https://golden-layout.com/files/latest/js/goldenlayout.min.js",
  "https://golden-layout.com/files/latest/css/goldenlayout-base.css",
  "https://golden-layout.com/files/latest/css/goldenlayout-dark-theme.css"
];

self.addEventListener("install", function(e) {
  console.log("[ServiceWorker] Install");
  e.waitUntil(
    caches.open(cacheName).then(function(cache) {
      console.log("[ServiceWorker] Caching app shell");
      return cache.addAll(filesToCache);
    })
  );
});

self.addEventListener("activate", function(e) {
  console.log("[ServiceWorker] Activate");
  e.waitUntil(
    caches.keys().then(function(keyList) {
      return Promise.all(
        keyList.map(function(key) {
          if (key !== cacheName) {
            console.log("[ServiceWorker] Removing old cache", key);
            return caches.delete(key);
          }
        })
      );
    })
  );
  return self.clients.claim();
});

self.addEventListener("fetch", function(e) {
  console.log("[ServiceWorker] Fetch", e.request.url);
  e.respondWith(
    // Cache then Network
    //caches.match(e.request).then(function(response) {
    //  return response || fetch(e.request);
    //})

    // Network then Cache
    fetch(e.request).catch(function() {
      return caches.match(e.request);
    })
  );
});
