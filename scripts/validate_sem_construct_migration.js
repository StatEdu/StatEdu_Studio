"use strict";

global.window = {};
require("../www/model-canvas/state.js");

const stateApi = window.StatEduModelCanvas.state;
const state = stateApi.create();
stateApi.restore(state, {
  modelSchemaVersion: 5,
  nodes: [
    { id: "legacyFactor", role: "latent", measurementMode: "reflective" },
    { id: "legacyComposite", role: "latent", measurementMode: "formative" },
    { id: "explicitUnknown", role: "latent", constructType: "unspecified", measurementMode: "reflective" },
    { id: "reflectiveComposite", role: "latent", constructType: "composite", measurementMode: "reflective", weightingMode: "modeA" },
    { id: "invalidFactor", role: "latent", constructType: "commonFactor", measurementMode: "formative", weightingMode: "auto" }
  ]
});

const byId = Object.fromEntries(state.nodes.map(node => [node.id, node]));
if (state.modelSchemaVersion !== 6) throw new Error("schema version was not upgraded");
if (byId.legacyFactor.constructType !== "commonFactor") throw new Error("legacy reflective factor was not inferred");
if (byId.legacyComposite.constructType !== "composite") throw new Error("legacy formative composite was not inferred");
if (byId.explicitUnknown.constructType !== "unspecified") throw new Error("explicit unspecified ontology was overwritten");
if (byId.explicitUnknown.advancedConstructSpecification) throw new Error("ordinary unspecified ontology should remain in the basic review flow");
if (!byId.reflectiveComposite.advancedConstructSpecification) throw new Error("reflective composite did not open advanced review");
if (!byId.invalidFactor.advancedConstructSpecification) throw new Error("incompatible factor specification did not open advanced review");
if (!byId.legacyFactor.constructSpecificationMigration.includes("construct_type_inferred")) throw new Error("migration provenance was not recorded");

const roundTrip = stateApi.create();
stateApi.restore(roundTrip, stateApi.snapshot(state));
if (roundTrip.nodes.find(node => node.id === "explicitUnknown").constructType !== "unspecified") throw new Error("round trip changed explicit unspecified ontology");

console.log("SEM construct migration validation passed.");
