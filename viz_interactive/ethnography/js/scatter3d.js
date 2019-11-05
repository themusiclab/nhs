var circularPoint = "data:image/png;base64,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eG1sbnM6c3RFdnQ9Imh0dHA6Ly9ucy5hZG9iZS5jb20veGFwLzEuMC9zVHlwZS9SZXNvdXJjZUV2ZW50IyIgeG1sbnM6ZGM9Imh0dHA6Ly9wdXJsLm9yZy9kYy9lbGVtZW50cy8xLjEvIiB4bWxuczpwaG90b3Nob3A9Imh0dHA6Ly9ucy5hZG9iZS5jb20vcGhvdG9zaG9wLzEuMC8iIHhtcDpDcmVhdG9yVG9vbD0iQWRvYmUgUGhvdG9zaG9wIENDIChNYWNpbnRvc2gpIiB4bXA6Q3JlYXRlRGF0ZT0iMjAxOS0wMS0yMVQxNTozMzo0MSswMzowMCIgeG1wOk1ldGFkYXRhRGF0ZT0iMjAxOS0wMS0yMVQxNTozMzo0MSswMzowMCIgeG1wOk1vZGlmeURhdGU9IjIwMTktMDEtMjFUMTU6MzM6NDErMDM6MDAiIHhtcE1NOkluc3RhbmNlSUQ9InhtcC5paWQ6NzI5YjZjYzMtNWNmNy00MzllLWEwNWEtZTkzYTdhY2M4NjlhIiB4bXBNTTpEb2N1bWVudElEPSJhZG9iZTpkb2NpZDpwaG90b3Nob3A6OGM0MWExZjUtMGE3NS1iNjRhLThlY2UtYTI0YjRlNjdlZmE3IiB4bXBNTTpPcmlnaW5hbERvY3VtZW50SUQ9InhtcC5kaWQ6NGU0ODQwM2UtNTYzNS00MmM4LTkxYzQtZjZlODIzMzk0MDg5IiBkYzpmb3JtYXQ9ImltYWdlL3BuZyIgcGhvdG9zaG9wOkNvbG9yTW9kZT0iMyIgcGhvdG9zaG9wOklDQ1Byb2ZpbGU9IkRpc3BsYXkiPiA8eG1wTU06SGlzdG9yeT4gPHJkZjpTZXE+IDxyZGY6bGkgc3RFdnQ6YWN0aW9uPSJjcmVhdGVkIiBzdEV2dDppbnN0YW5jZUlEPSJ4bXAuaWlkOjRlNDg0MDNlLTU2MzUtNDJjOC05MWM0LWY2ZTgyMzM5NDA4OSIgc3RFdnQ6d2hlbj0iMjAxOS0wMS0yMVQxNTozMzo0MSswMzowMCIgc3RFdnQ6c29mdHdhcmVBZ2VudD0iQWRvYmUgUGhvdG9zaG9wIENDIChNYWNpbnRvc2gpIi8+IDxyZGY6bGkgc3RFdnQ6YWN0aW9uPSJzYXZlZCIgc3RFdnQ6aW5zdGFuY2VJRD0ieG1wLmlp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";
var rectangularPoint = "data:image/png;base64,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eG1sbnM6c3RFdnQ9Imh0dHA6Ly9ucy5hZG9iZS5jb20veGFwLzEuMC9zVHlwZS9SZXNvdXJjZUV2ZW50IyIgeG1sbnM6ZGM9Imh0dHA6Ly9wdXJsLm9yZy9kYy9lbGVtZW50cy8xLjEvIiB4bWxuczpwaG90b3Nob3A9Imh0dHA6Ly9ucy5hZG9iZS5jb20vcGhvdG9zaG9wLzEuMC8iIHhtcDpDcmVhdG9yVG9vbD0iQWRvYmUgUGhvdG9zaG9wIENDIChNYWNpbnRvc2gpIiB4bXA6Q3JlYXRlRGF0ZT0iMjAxOS0wMS0yM1QwMDoxMDo0OCswMzowMCIgeG1wOk1ldGFkYXRhRGF0ZT0iMjAxOS0wMS0yM1QwMDoxMDo0OCswMzowMCIgeG1wOk1vZGlmeURhdGU9IjIwMTktMDEtMjNUMDA6MTA6NDgrMDM6MDAiIHhtcE1NOkluc3RhbmNlSUQ9InhtcC5paWQ6MTA2YzQ2MGEtYTAyMy00OGIxLTliN2MtNGIyOTMzODNjYjQ4IiB4bXBNTTpEb2N1bWVudElEPSJhZG9iZTpkb2NpZDpwaG90b3Nob3A6NDZlMjU0MWUtYzhlNC0yMDQ0LTgzNzMtMzhmYWY2YzY5ODAxIiB4bXBNTTpPcmlnaW5hbERvY3VtZW50SUQ9InhtcC5kaWQ6NjhiNzgzOGYtN2NhNy00MjA3LTg4NTMtNWFjNDkxYjg4NDVhIiBkYzpmb3JtYXQ9ImltYWdlL3BuZyIgcGhvdG9zaG9wOkNvbG9yTW9kZT0iMyIgcGhvdG9zaG9wOklDQ1Byb2ZpbGU9IkRpc3BsYXkiPiA8eG1wTU06SGlzdG9yeT4gPHJkZjpTZXE+IDxyZGY6bGkgc3RFdnQ6YWN0aW9uPSJjcmVhdGVkIiBzdEV2dDppbnN0YW5jZUlEPSJ4bXAuaWlkOjY4Yjc4MzhmLTdjYTctNDIwNy04ODUzLTVhYzQ5MWI4ODQ1YSIgc3RFdnQ6d2hlbj0iMjAxOS0wMS0yM1QwMDoxMDo0OCswMzowMCIgc3RFdnQ6c29mdHdhcmVBZ2VudD0iQWRvYmUgUGhvdG9zaG9wIENDIChNYWNpbnRvc2gpIi8+IDxyZGY6bGkgc3RFdnQ6YWN0aW9uPSJzYXZlZCIgc3RFdnQ6aW5zdGFuY2VJRD0ieG1wLmlp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";

var CSV_PREFIX = "data/";
var CSV_URL = "ethno_bpca_scores_highlighted.csv";
var REFERENCE_URL = "reference_points.csv";

var DATASET_PREFIX = "data/dataset/";
var DATASET_URLS = ["NHSEthnography_AnnotatePrim.csv",
		    "NHSEthnography_AnnotateSec.csv",
		    "NHSEthnography_FreeText_cleaned.csv",
		    "NHSEthnography_Scraping.csv",
		    "NHSCultures_Metadata.csv",
		    "NHSEthnography_Metadata.csv"];

var ethno = {

    index: "indx",

    x: "score_1", 
    y: "score_2",
    z: "score_3",

    xAlias : "PC1: Formality",
    yAlias : "PC2: Affect",
    zAlias : "PC3: Religiosity",
    
    title : "NHS Ethnography Explorer",

    indices: {

        NHSEthnography_AnnotatePrim: "indx",
        NHSEthnography_AnnotateSec: "indx",
        NHSEthnography_FreeText_cleaned: "indx",
        //NHSEthnography_Reliability : "indx", 
        NHSEthnography_Scraping: "indx",
        NHSCultures_Metadata: "id_nhs",
        NHSEthnography_Metadata: "NHSCultures_Metadata.id_hraf",

    },

    color: [

        {
            index: "na",
            option: "NA",
            dict: ["na"],
            colors: ["#6D98F3", "#E96465", "#F9C659", "#6AB878", "#D3D3D3"],
            f: function () {
                return 0;
            }

        },

        {
            index: "song_function",
            option: "Song Type",
            dict: ["dance", "healing", "love", "lullaby", "other"],
            colors: ["#6D98F3", "#E96465", "#F9C659", "#6AB878", "#D3D3D3"],
            f: function (n_) {

                var found = 0;
                if (n_ == "." || n_ == "") {
                    return -1;
                }
                this.dict.forEach(function (d_, i_) {

                    var isEquel = JSON.stringify(n_) === JSON.stringify(d_);
                    if (isEquel) {
                        found = i_;
                    }

                });

                return found;
            }
        },
        {
            index: "NHSEthnography_AnnotateSec.audience_sex",
            option: "Sex of Audience Member(s)",
            dict: ["Male", "Female", "Both sexes"],
            colors: ["#6D98F3", "#E96465", "#F9C659", "#6AB878", "#D3D3D3"],
            f: function (n_) {

                var found = 0;
                if (n_ == "." || n_ == "") {
                    return -1;
                }
                this.dict.forEach(function (d_, i_) {

                    var isEquel = JSON.stringify(n_) === JSON.stringify(d_);
                    if (isEquel) {
                        found = i_;
                    }

                });

                return found;
            }
        },
        {
            index: "NHSEthnography_AnnotatePrim.singers_sex",
            option: "Sex of Singer(s)",
            dict: ["Male", "Female", "Both sexes"],
            colors: ["#6D98F3", "#E96465", "#F9C659", "#6AB878", "#D3D3D3"],
            f: function (n_) {

                var found = 0;
                if (n_ == "." || n_ == "") {
                    return -1;
                }
                this.dict.forEach(function (d_, i_) {

                    var isEquel = JSON.stringify(n_) === JSON.stringify(d_);
                    if (isEquel) {
                        found = i_;
                    }

                });

                return found;
            }
        },
        {
            index: "NHSEthnography_AnnotatePrim.instrument",
            option: "Musical Instrument(s)",
            dict: ["non-instrumental", "instrumental"],
            colors: ["#6D98F3", "#E96465", "#F9C659", "#6AB878", "#D3D3D3"],
            f: function (n_) {
                if (n_ == "." || n_ == "") {
                    return -1;
                }
                return n_;
            }
        },
        {
            index: "NHSEthnography_AnnotatePrim.ceremony",
            option: "Ceremony",
            dict: ["non-ceremonical", "ceremonical"],
            colors: ["#6D98F3", "#E96465", "#F9C659", "#6AB878", "#D3D3D3"],
            f: function (n_) {
                if (n_ == "." || n_ == "") {
                    return -1;
                }
                return n_;
            }
        },
        {
            index: "NHSEthnography_AnnotatePrim.trance",
            option: "Trance",
            dict: ["no trance", "trance"],
            colors: ["#6D98F3", "#E96465", "#F9C659", "#6AB878", "#D3D3D3"],
            f: function (n_) {
                if (n_ == "." || n_ == "") {
                    return -1;
                }
                return n_;
            }
        },
        {
            index: "NHSEthnography_AnnotatePrim.child_for",
            option: "Infant/child-directed singing",
            dict: ["not for child", "for child"],
            colors: ["#6D98F3", "#E96465", "#F9C659", "#6AB878", "#D3D3D3"],
            f: function (n_) {
                if (n_ == "." || n_ == "") {
                    return -1;
                }
                return n_;
            }
        },
        {
            index: "NHSEthnography_AnnotatePrim.audience_dance",
            option: "Dancing by Audience Member(s)",
            dict: ["audience didn't dance", "audience danced"],
            colors: ["#6D98F3", "#E96465", "#F9C659", "#6AB878", "#D3D3D3"],
            f: function (n_) {
                if (n_ == "." || n_ == "") {
                    return -1;
                }
                return n_;
            }
        },
        {
            index: "NHSEthnography_AnnotatePrim.singers_dance",
            option: "Dancing by Singer(s)",
            dict: ["singer(s) didn't dance", "singer(s) danced"],
            colors: ["#6D98F3", "#E96465", "#F9C659", "#6AB878", "#D3D3D3"],
            f: function (n_) {
                if (n_ == "." || n_ == "") {
                    return -1;
                }
                return n_;
            }
        },

    ],

    radius: [

        {
            index: "na",
            option: "(No size variable selected)",
            dict: [{
                l: "All instances of song",
                r: 4.9
            }],
            f: function (n_, key_) {

                if (n_ == "." || n_ == "") return -1;
                if (key_ == "l") {
                    return "na";
                } else if (key_ == "r") {
                    return this.dict[0].r;
                } else if (key_ == "t") {
                    return 1;
                } else return 0;

            }
        },
        {
            index: "NHSEthnography_AnnotateSec.recur",
            option: "Recurrence of Singing",
            dict: [{
                l: "No recurrence",
                r: 2.0,
                t: 1
            }, {
                l: "1-2 days",
                r: 5.0,
                t: 1
            }, {
                l: "3-7 days",
                r: 10.0,
                t: 1
            }, {
                l: ">7 days",
                r: 15.0,
                t: 1
            }],
            f: function (n_, key_) {

                var found = 0;
                if (n_ == "." || n_ == "") return -1;
                this.dict.forEach(function (d_, i_) {

                    var isEquel = JSON.stringify(n_) === JSON.stringify(d_.l);
                    if (isEquel) {

                        if (key_ == "i") {
                            found = i_;
                        } else {
                            found = d_[key_];
                        }
                    }

                });

                return found;
            }
        },
        {
            index: "NHSEthnography_AnnotateSec.time_start",
            option: "Time of Singing",
            dict: [{
                l: "Early morning (0400 to 0700) (includes daybreak, dawn)",
                r: 4.0,
                t: 1
            }, {
                l: "Morning (0700 to 1000)",
                r: 6.0,
                t: 1
            }, {
                l: "Midday (1000 to 1400)",
                r: 8.0,
                t: 1
            }, {
                l: "Afternoon (1400 to 1700)",
                r: 10.0,
                t: 1
            }, {
                l: "Early evening (1700 to 1900)",
                r: 12.0,
                t: 1
            }, {
                l: "Evening (1900 to 2200)",
                r: 16.0,
                t: 1
            }, {
                l: "Night (2200 to 0400)",
                r: 18.0,
                t: 1
            }],
            f: function (n_, key_) {

                var found = 0;
                if (n_ == "." || n_ == "") return -1;
                this.dict.forEach(function (d_, i_) {

                    var isEquel = JSON.stringify(n_) === JSON.stringify(d_.l);
                    if (isEquel) {

                        if (key_ == "i") {
                            found = i_;
                        } else {
                            found = d_[key_];
                        }
                    }

                });

                return found;
            }
        },
        {
            index: "NHSEthnography_AnnotateSec.duration",
            option: "Duration of Singing",
            dict: [{
                l: "<10 min",
                r: 3.0,
                t: 1
            }, {
                l: "10 min-1 hour",
                r: 6.0,
                t: 1
            }, {
                l: "1-10 hours",
                r: 9.0,
                t: 1
            }, {
                l: ">10 hours",
                r: 12.0,
                t: 1
            }],
            f: function (n_, key_) {

                var found = 0;
                if (n_ == "." || n_ == "") return -1;
                this.dict.forEach(function (d_, i_) {

                    var isEquel = JSON.stringify(n_) === JSON.stringify(d_.l);
                    if (isEquel) {

                        if (key_ == "i") {
                            found = i_;
                        } else {
                            found = d_[key_];
                        }
                    }

                });

                return found;
            }
        },
        {
            index: "NHSEthnography_AnnotateSec.audience_age2",
            option: "Age of Audience Member(s)",
            dict: [{
                l: "Child",
                r: 2.0,
                t: 1
            }, {
                l: "Adolescent/young adult",
                r: 4.0,
                t: 1
            }, {
                l: "Adult",
                r: 6.0,
                t: 1
            }, {
                l: "Elder",
                r: 8.0,
                t: 1
            }, {
                l: "All ages",
                r: 10.0,
                t: 1
            }],
            f: function (n_, key_) {

                var found = 0;
                if (n_ == "." || n_ == "") return -1;
                this.dict.forEach(function (d_, i_) {

                    var isEquel = JSON.stringify(n_) === JSON.stringify(d_.l);
                    if (isEquel) {

                        if (key_ == "i") {
                            found = i_;
                        } else {
                            found = d_[key_];
                        }
                    }

                });

                return found;
            }
        },
        {
            index: "NHSEthnography_AnnotateSec.audience_n",
            option: "Number of Audence Members",
            dict: [{
                l: "Solo listener",
                r: 2.0,
                t: 1
            }, {
                l: "2-5 listeners",
                r: 5.0,
                t: 1
            }, {
                l: "6-10 listeners",
                r: 10.0
            }, {
                l: "11-20 listeners",
                r: 15.0,
                t: 1
            }, {
                l: "21-30 listeners",
                r: 25.0,
                t: 1
            }, {
                l: "30-60 listeners",
                r: 45.0,
                t: 1
            }, {
                l: "60-100 listeners",
                r: 75.0,
                t: 1
            }, {
                l: ">100 listeners",
                r: 100.0,
                t: 1
            }],
            f: function (n_, key_) {

                var found = 0;
                if (n_ == "." || n_ == "") return -1;
                this.dict.forEach(function (d_, i_) {

                    var isEquel = JSON.stringify(n_) === JSON.stringify(d_.l);
                    if (isEquel) {

                        if (key_ == "i") {
                            found = i_;
                        } else {
                            found = d_[key_];
                        }
                    }

                });

                return found;
            }
        },
        {
            index: "NHSEthnography_AnnotateSec.singers_age1",
            option: "Age of Singer(s)",
            dict: [{
                l: "Child",
                r: 2.0,
                t: 1
            }, {
                l: "Adolescent/young adult",
                r: 4.0,
                t: 1
            }, {
                l: "Adult",
                r: 6.0,
                t: 1
            }, {
                l: "Elder",
                r: 8.0,
                t: 1
            }, {
                l: "All ages",
                r: 10.0,
                t: 1
            }],
            f: function (n_, key_) {

                var found = 0;
                if (n_ == "." || n_ == "") return -1;
                this.dict.forEach(function (d_, i_) {

                    var isEquel = JSON.stringify(n_) === JSON.stringify(d_.l);
                    if (isEquel) {

                        if (key_ == "i") {
                            found = i_;
                        } else {
                            found = d_[key_];
                        }
                    }

                });

                return found;
            }
        },
        {
            index: "NHSEthnography_AnnotateSec.singers_n",
            option: "Number of Singers",
            dict: [{
                    l: "Solo singer",
                    r: 2.0,
                    t: 1
                }, {
                    l: "2-5 singers",
                    r: 5.0,
                    t: 1
                }, {
                    l: "6-10 singers",
                    r: 10.0,
                    t: 1
                }, {
                    l: "11-20 singers",
                    r: 15.0,
                    t: 1
                }, {
                    l: "21-30 singers",
                    r: 25.0,
                    t: 1
                },
                {
                    l: "30-60 singers",
                    r: 45.0,
                    t: 1
                }, {
                    l: "60-100 singers",
                    r: 75.0,
                    t: 1
                }, {
                    l: ">100 singers",
                    r: 100.0,
                    t: 1
                }
            ],
            f: function (n_, key_) {

                var found = 0;
                if (n_ == "." || n_ == "") return -1;
                this.dict.forEach(function (d_, i_) {

                    var isEquel = JSON.stringify(n_) === JSON.stringify(d_.l);
                    if (isEquel) {

                        if (key_ == "i") {
                            found = i_;
                        } else {
                            found = d_[key_];
                        }
                    }

                });

                return found;
            }
        }

    ],

    popup: [

        {
            index: "NHSCultures_Metadata.culture"
        },
        {
            index: "NHSEthnography_FreeText_cleaned.text"
        },
        {
            index: "NHSEthnography_FreeText_cleaned.lyric"
        },
        {
            index: "NHSEthnography_Scraping.cite_text"
        },
        {
            index: "NHSEthnography_Scraping.ocm"
        }

    ],

    checkbox: {

        legend: ["Show/Hide"],
        dictionary: {

            "EmberÃ¡": "Emberá",
            "JavaÃ©": "Javaé"

        },

        cultures: [],
        regions: [],
        index: [],
        dict : {
            
            culture : "Society",
            text : "Ethnographic Text",
            lyric : "Lyrics",
            cite_text : "Citation",
            ocm : "OCM Identifier(s)"

        }

    }

};

var parameters = {
    
    references: [],
    referenceDimensions: {r: 8.0},
    referenceColorsFromData: false,
    referenceDarkerRatio: 1.0,
    popup: ["text", "lyric", "cite_text", "ocm"],
    
    defaultAlpha : 0.5,         //for all points except highlighted & referenced
    defaultAlpha2 : 0.1,       //all points on highlighted mode ON
    
    highlightedAlpha : 0.9,
    highlightedDarker : 1.2,
    filtered : false,

    color: ethno.color[1],
    radius: ethno.radius[0],

    na_color: "#808080",

    radiusGauge: 5.0,
    dynamicGauge: 2.5E-2, //dynamic value, please don't touch
    ratio: 32.0,

    zoomLimits: {
        min: 16,
        max: 1024
    },
    spacePanning: false,

    dimensions: {
        x: {
            min: Number.POSITIVE_INFINITY,
            max: Number.NEGATIVE_INFINITY
        },
        y: {
            min: Number.POSITIVE_INFINITY,
            max: Number.NEGATIVE_INFINITY
        },
        z: {
            min: Number.POSITIVE_INFINITY,
            max: Number.NEGATIVE_INFINITY
        },
    },

    gridColor: "#DEDEDE",
    gridSize: 147.0,
    gridMarkup: 16, //# of cols/rows
    axisValuesFontSize: "10px",

    //radiusScale: d3.scaleLinear().domain([1, 10]).range([2, 10]),
    //radiusScale: d3.scaleSqrt().domain([1, 10]).range([3, 20]),
    radiusScale: d3.scaleLinear().domain([1, 10]).range([2, 20]),
    radiusRange: {
        1: {
            min: Number.POSITIVE_INFINITY,
            max: Number.NEGATIVE_INFINITY
        },
        2: {
            min: Number.POSITIVE_INFINITY,
            max: Number.NEGATIVE_INFINITY
        }
    },

    scalable: true,   //scale on/off axes
    mousemode: "Drag", //drag or pan
    active: false,    //dragging mode on/off
    shift: false,
    centroid: false,  //centroid mode (mass centre at median value)

    statsOn: false

};

makeEvenRadiuses(ethno.radius);

var stats, scene, renderer, controls, dragControls, lastCamera, lastPosition;
var camera, cameraControl, axesLabels, buffer, uniforms, mouse, raycaster, inverseMatrix, ray, shift = false, pickingScene, pickingTexture, clickable = true, lastID = 0;

var pointCloud, points = [],
    last_id = 0,
    reset_id = 0,
    sorted = [],
    axes = [],
    values = [],
    labels = [],
    grids = [],
    guide;

var offset = new THREE.Vector3();

var limits = {
    x: {
        min: Number.POSITIVE_INFINITY,
        max: Number.NEGATIVE_INFINITY
    },
    y: {
        min: Number.POSITIVE_INFINITY,
        max: Number.NEGATIVE_INFINITY
    },
    z: {
        min: Number.POSITIVE_INFINITY,
        max: Number.NEGATIVE_INFINITY
    }
}

var svg, div, divState, legendBox, legendParameters, w, h;

var radiusRatio = 2.5E-2;

var w = window.innerWidth,
    h = window.innerHeight;

var xAxis, yAxis, zAxis, xScale, yScale, zScale, xPath, yPath, zPath;
var scalable = false;
var gridVisible = false;
var db = [];
var rendering = true;

d3.csv(CSV_PREFIX + CSV_URL, function (error_, data_) {

    if (error_) throw error_;
    setSelectors();

    data_.forEach(function (d_) {
        points[d_[ethno.index]] = d_;

        limits.x.min = Math.min(d_[ethno.x], limits.x.min);
        limits.x.max = Math.max(d_[ethno.x], limits.x.max);

        limits.y.min = Math.min(d_[ethno.y], limits.y.min);
        limits.y.max = Math.max(d_[ethno.y], limits.y.max);

        limits.z.min = Math.min(d_[ethno.z], limits.z.min);
        limits.z.max = Math.max(d_[ethno.z], limits.z.max);

    });

    loadAndProcessDataset();

});

function setSelectors() {

    var selector1 = document.getElementById("colorSelector");
    ethno.color.forEach(function (d_) {

        var option = document.createElement("option");
        option.text = d_.option;
        option.value = d_.index;
        selector1.add(option);

    });

    var selector2 = document.getElementById("radiusSelector");
    ethno.radius.forEach(function (d_) {

        var option = document.createElement("option");
        option.value = d_.index;
        option.text = d_.option;
        selector2.add(option);

    });
    
    document.getElementById("colorSelector").options.selectedIndex = 1;

}

function loadAndProcessDataset() {

    var queue = d3.queue();

    DATASET_URLS.forEach(function (url_) {
        queue.defer(d3.csv, DATASET_PREFIX + url_)
    });

    queue.awaitAll(function (error_, dataset_) {

        if (error_) throw error_;

        dataset_.forEach(function (d_, i_) {

            var name = DATASET_URLS[i_].replace(".csv", "").trim();
            db[name] = [];

            d_.forEach(function (e_) {

                db[name][e_[Object.keys(e_)[0]]] = e_;

            });

        });

        restructurePoints(points, db);
        setCulturesPopup(db);

        svg = d3.select("#scatter3D").append("svg").attr("width", w).attr("height", h);
        div = d3.select("#tooltip");
        divState = false;

        var defs = svg.append("defs")
            .append("pattern")
            .attr("id", "cross")
            .attr("patternUnits", "userSpaceOnUse")
            .attr("x", 3)
            .attr("y", 2)
            .attr("width", 12)
            .attr("height", 12)
            .append("svg:path")
            .attr("d", "M 4 4 L 10 10 M 4 10 L 10 4")
            .attr("stroke", "#000000");
	
        var defs = svg.append("defs")
            .append("pattern")
            .attr("id", "mark")
            .attr("patternUnits", "userSpaceOnUse")
            .attr("x", 4)
            .attr("y", 3)
            .attr("width", 12)
            .attr("height", 12)
            .append("svg:path")
            .attr("d", "M 0 6 L 12 6 M 6 0 L 6 12")
            .attr("stroke", "#000000");

        renderLegend();

        document.addEventListener("keydown", function (event_) {

            if (event_.shiftKey) {
                parameters.shift = true;
                switchControls("shiftDown");
            }
        });

        document.addEventListener("keyup", function (event_) {
            switchControls();
        });
        window.addEventListener('resize', onWindowResize, false);

        loadReferencePoints();	

    })

}

 function loadReferencePoints(){
     
      d3.csv(CSV_PREFIX + REFERENCE_URL, function (error_, data_) {

      if (error_) throw error_;
          
          data_.forEach(function(d_){ parameters.references.push(d_); })
          
          var loader = new THREE.FontLoader();

          loader.load("fonts/helvetiker_regular.typeface.json", function (font_) { init(font_); });
          
          
      });
 }


function renderLegend() {

    d3.selectAll(".legend").remove();

    legendParameters = {

        x: document.getElementById("ui").offsetLeft,
        y: document.getElementById("ui").offsetTop + document.getElementById("ui").offsetHeight + 16,
        width: document.getElementById("ui").offsetWidth,
        height: 180,
        fontSize: "10px",
        header1: {
            x: 16,
            y: 24
        },
        header2: {
            x: 16,
            y: 75
        },
        header3: {
            x: 16,
            y: 140
        },
        carriage: 0,

        maxLength: 20,
        separation: 12,
        increment: 12,

    }


    legendBox = svg.append("g").attr("id", "legendBox").attr("transform", function () {
        return "translate(" + (legendParameters.x) + ", " + legendParameters.y + ")";
    })

    var legendBackground = legendBox.append("rect")
        .attr("class", "legendBox legend legendBackground")
        .attr("x", 0)
        .attr("y", 0)
        .attr("width", legendParameters.width)
        .attr("height", legendParameters.height)

    var cLabel = legendBox.append("text").attr("class", "legend").attr("transform", "translate(0," + (legendParameters.header1.y - legendParameters.separation) + ")").style("font-family", "sans-serif").attr("font-size", legendParameters.fontSize).attr("font-weight", "bold").each(function (d_) {

        var textX = 0;
        var lines = splitToMultiLines(parameters.color.option, legendParameters.maxLength).split('\n');
        for (var i = 0; i < lines.length; i++) {
            d3.select(this)
                .append("tspan")
                .attr("dx", legendParameters.header1.x)
                .attr("dy", legendParameters.separation)
                .attr("x", textX)
                .text(lines[i]);
        }
    });

    legendParameters.height += legendParameters.increment * (Math.floor(parameters.color.option.length / 20));
    legendParameters.header2.y += legendParameters.increment * (Math.floor(parameters.color.option.length / 20));
    legendParameters.header3.y += legendParameters.increment * (Math.floor(parameters.color.option.length / 20));

    var legendList = legendBox.selectAll(".legendList").data(parameters.color.dict)
        .enter()
        .append("g")
        .attr("transform", function (d_, i_) {
            return "translate(38," + (48 + i_ * 18 + (legendParameters.increment * (Math.floor(parameters.color.option.length / 20)))) + ")";
        })
        .attr("class", "legendList legend")

    legendList.append("rect")
        .attr("class", "legend")
        .attr("id", function (d_, i_) {
            return "blob_" + i_;
        }, true)
        .attr("x", -20)
        .attr("y", -11)
        .attr("rx", 6)
        .attr("ry", 6)
        .attr("width", 12)
        .attr("height", 12)
        .attr("stroke-width", 2.5)
        .attr("opacity", 0.69)
        .attr("fill", function (d_, i_) {


            legendParameters.height += d3.select(this).node().getBBox().height * 1.1;
            legendParameters.header2.y += d3.select(this).node().getBBox().height * 1.1;
            legendParameters.header3.y += d3.select(this).node().getBBox().height * 1.1;

            if (parameters.color.index == "na") {
                return parameters.na_color;
            } else {
                return parameters.color.colors[i_];
            }
        })

    legendList.append("text")
        .attr("class", function (d_, i_) {
            return "legend colorLabel_" + i_
        })
        .attr("baseline", "central")
        .style("font-family", "sans-serif")
        .style("font-size", legendParameters.fontSize)
        .text(function (d_) {
            return d_;
        });

    legendList.append("rect")
        .attr("class", "legend")
        .attr("x", -26)
        .attr("y", -14)
        .attr("width", legendParameters.width - 16)
        .attr("height", 18)
        .attr("fill", "transparent")
        .on("mouseover", function (d_, i_) {

            d3.select(".colorLabel_" + i_).attr("font-weight", "bolder");

        })
        .on("mouseout", function (d_, i_) {

            d3.select(".colorLabel_" + i_).attr("font-weight", "normal");

        })
        .on("click", function (d_, i_) {

            var r = d3.select("#blob_" + i_);

            if (r.attr("fill") == "#DEDEDE") {

                if (parameters.color.index == "na") {
                    r.attr("fill", parameters.na_color);
                } else {
                    r.attr("fill", parameters.color.colors[i_]);
                }


                for (var j = 0; j < points.length; j++) {


                    if (buffer.color_indx[j] == i_) {

                        buffer.visibility[j * 3] = 1.;
                        pointCloud.geometry.attributes.visibility.needsUpdate = true;

                    }

                }


            } else {

                r.attr("fill", "#DEDEDE");


                for (var j = 0; j < points.length; j++) {


                    if (buffer.color_indx[j] == i_) {

                        buffer.visibility[j * 3] = 0.;
                        pointCloud.geometry.attributes.visibility.needsUpdate = true;

                    }

                }

            }

        });

    var rLabel = legendBox.append("text").attr("class", "legend").attr("transform", "translate(0," + (legendParameters.header2.y - legendParameters.separation) + ")").style("font-family", "sans-serif").attr("font-size", legendParameters.fontSize).attr("font-weight", "bold").each(function (d_) {

        var textX = 0;
        var lines = splitToMultiLines(parameters.radius.option, legendParameters.maxLength).split('\n');
        for (var i = 0; i < lines.length; i++) {
            d3.select(this)
                .append("tspan")
                .attr("dx", legendParameters.header2.x)
                .attr("dy", legendParameters.separation)
                .attr("x", textX)
                .text(lines[i]);
        }
    });

    legendParameters.carriage = legendParameters.header2.y + 8 + legendParameters.increment * (Math.floor(parameters.radius.option.length / 20));
    //legendParameters.header3.y += legendParameters.increment * (Math.floor(parameters.radius.option.length / 20));
    legendParameters.height.y += 16;
    legendParameters.header2.y += 16;

    var legendList2 = legendBox.selectAll(".legendList2").data(parameters.radius.dict)
        .enter()
        .append("g")
        .attr("transform", function (d_) {
            legendParameters.carriage += d_.r * 2.2;
            return "translate(56," + (legendParameters.carriage) + ")";
        })
        .attr("class", "legendList2 legend")

    legendList2.append("rect")
        .attr("class", "legend")
        .attr("id", function (d_, i_) {

            return "rad_" + i_;
        })
        .attr("x", function (d_) {
            if (d_.t != 2) {
                return -20 - opticalCompensation(d_.r);
            } else {
                return -23 - opticalCompensation(d_.r) * 0.75
            }
        })
        .attr("y", function (d_) {
            if (d_.t != 2) {
                return -d_.r;
            } else {
                return -d_.r * 0.75
            }
        })
        .attr("rx", function (d_) {
            if (d_.t != 2) {
                return d_.r;
            } else {
                return d_.r / 3;
            }
        })
        .attr("ry", function (d_) {
            if (d_.t != 2) {
                return d_.r;
            } else {
                return d_.r / 3;
            }
        })
        .attr("width", function (d_) {
            if (d_.t != 2) {
                return d_.r * 2;
            } else {
                return d_.r * 2 * 0.75;
            }
        })
        .attr("height", function (d_) {
            if (d_.t != 2) {
                return d_.r * 2;
            } else {
                return d_.r * 2 * 0.75;
            }
        })
        .attr("fill", "#FFFFFF")
        .attr("stroke", "#000000")
        .attr("transform", function (d_) {

            legendParameters.height += d3.select(this).node().getBBox().height * 1.1;
            legendParameters.header3.y += d3.select(this).node().getBBox().height * 1.1;

            if (d_.t == 2) {
                return "Rotate(" + (-45) + "," + (-23 - opticalCompensation(d_.r * 0.75) + d_.r * 0.75) + "," + (0) + ")";
            }
        });

    legendList2.append("text")
        .attr("class", function (d_, i_) {
            return "legend radiusLabel_" + i_;
        })
        .attr("id", function (d_, i_) {
            return "scale_" + i_;
        })
        .attr("baseline", "central")
        .attr("dx", -16)
        .attr("dy", 3)
        .style("font-family", "sans-serif")
        .style("font-size", legendParameters.fontSize)
        .text(function (d_) {
            return d_.l.replace(" (includes daybreak, dawn)", "");
        });

    legendList2.append("rect")
        .attr("class", "legend")
        .attr("x", -48)
        .attr("y", function (d_) {
            return -d_.r * 1.1;
        })
        .attr("width", legendParameters.width - 16)
        .attr("height", function (d_) {
            return d_.r * 2.2;
        })
        .attr("fill", "transparent")
        .on("mouseover", function (d_, i_) {

            d3.select(".radiusLabel_" + i_).attr("font-weight", "bolder");

        })
        .on("mouseout", function (d_, i_) {

            d3.select(".radiusLabel_" + i_).attr("font-weight", "normal");

        })
        .on("click", function (d_, i_) {

            var r = d3.select("#rad_" + i_);

            if (r.attr("fill") == "transparent") {

                r.attr("fill", "#FFFFFF");

                for (var j = 0; j < points.length; j++) {


                    if (buffer.radius_indx[j] == i_) {

                        buffer.visibility[j * 3 + 1] = 1.;
                        pointCloud.geometry.attributes.visibility.needsUpdate = true;

                    }

                }


            } else {

                r.attr("fill", "transparent");
                r.attr("stroke", "#DEDEDE");

                for (var j = 0; j < points.length; j++) {


                    if (buffer.radius_indx[j] == i_) {

                        buffer.visibility[j * 3 + 1] = 0.;
                        pointCloud.geometry.attributes.visibility.needsUpdate = true;

                    }

                }

            }

        });


    var bLabel = legendBox.append("text").attr("class", "legend").attr("dx", legendParameters.header3.x).attr("dy", legendParameters.header3.y).style("font-family", "sans-serif").attr("font-size", legendParameters.fontSize).attr("font-weight", "bold").text("Societies");

    var legendList3 = legendBox.selectAll(".legendList3").data(ethno.checkbox.legend)
        .enter()
        .append("g")
        .attr("transform", function (d_, i_) {
            return "translate(38," + (legendParameters.header3.y + 24 + i_ * 18) + ")";
        })
        .attr("class", "legendList3 legend")

    legendList3.append("rect")
        .attr("class", "legend")
        .attr("id", function (d_, i_) {
            return "blob_" + i_;
        }, true)
        .attr("x", -20)
        .attr("y", -9)
        .attr("rx", 6)
        .attr("ry", 6)
        .attr("width", 12)
        .attr("height", 12)
        .attr("stroke-width", 2.5)
        .attr("opacity", 0.69)
        .attr("fill", "url(#cross)")
        .attr("stroke", "#000000")
        .attr("stroke-width", 1.2)

    legendList3.append("text")
        .attr("class", "legend checkLabel")
        .attr("baseline", "central")
        .style("font-family", "sans-serif")
        .style("font-size", legendParameters.fontSize)
        .text(function (d_) {
            return d_;
        });

    legendList3.append("rect")
        .attr("class", "legend")
        .attr("x", -26)
        .attr("y", -15)
        .attr("width", 128)
        .attr("height", 24)
        .attr("fill", "transparent")
        .on("mouseover", function (d_) {

            d3.select(".checkLabel").attr("font-weight", "bolder");

        })
        .on("mouseout", function (d_) {

            d3.select(".checkLabel").attr("font-weight", "normal");

        })
        .on("click", function (d_) {

            rendering = false;
            clickable = false;
            var p = document.getElementById("culturesPopup");
            p.style.display = "grid";

        });

    d3.select(".legendBackground").attr("height", legendParameters.height)

}

function init(font_) {
    
    var xPath = svg.append("defs").append("path").attr("id", "xPath");
    var yPath = svg.append("defs").append("path").attr("id", "yPath");
    var zPath = svg.append("defs").append("path").attr("id", "zPath");

    var xLabel = svg.append("text").append("textPath")
    .attr("class", "xLabel")
	.attr("xlink:href", "#xPath")
    .attr("text-anchor", "middle")
    .attr("font-family", "sans-serif")
    .attr("font-size", "12px")
    .attr("pointer-events", "none")
    .attr("startOffset", "66%")
    .attr("dominant-baseline","ideographic")
	.text(ethno.xAlias);
    
    var yLabel = svg.append("text").append("textPath")
    .attr("class", "yLabel")
	.attr("xlink:href", "#yPath")
    .attr("text-anchor", "end")
    .attr("font-family", "sans-serif")
    .attr("font-size", "12px")
    .attr("pointer-events", "none")
    .attr("startOffset", "56%")
    .attr("dominant-baseline","ideographic")
	.text(ethno.yAlias);
    
    var zLabel = svg.append("text").append("textPath")
    .attr("class", "zLabel")
	.attr("xlink:href", "#zPath")
    .attr("text-anchor", "middle")
    .attr("font-family", "sans-serif")
    .attr("font-size", "12px")
    .attr("pointer-events", "none")
    .attr("startOffset", "66%")
    .attr("dominant-baseline","ideographic")
	.text(ethno.zAlias);

    var title = svg.append("text")
    .attr("transform", "translate(" + (legendParameters.width + legendParameters.x + 32) + ", 45)")
    .attr("font-size", 24)
    .attr("font-family", "sans-serif")
    .text(ethno.title);
    
    xScale = d3.scaleLinear().range([0, w * 0.25]).domain([limits.x.min, limits.x.max]);

    renderer = new THREE.WebGLRenderer();
    renderer.setPixelRatio(window.devicePixelRatio);
    renderer.setSize(w, h);
    renderer.setClearColor(0xFFFFFF);
    document.body.appendChild(renderer.domElement);
    renderer.domElement.id = "threejs";

    if (parameters.statsOn) {

        stats = new Stats();
        stats.domElement.style.position = "absolute";
        stats.domElement.style.bottom = "0px";
        document.body.appendChild(stats.domElement);

    }

    scene = new THREE.Scene();

    camera = new THREE.PerspectiveCamera(35, w / h, 1, 10000);
    camera.position.set(256, 512, 512);
    camera.lookAt(0, 0, 0)
    scene.add(camera);

    pickingScene = new THREE.Scene();
    pickingTexture = new THREE.WebGLRenderTarget(window.innerWidth * window.devicePixelRatio, window.innerHeight * window.devicePixelRatio);
    pickingTexture.texture.minFilter = THREE.LinearFilter;
    
    buffer = {

        id: new Float32Array(points.length + parameters.references.length),
        popup: new Float32Array(points.length * 3 + parameters.references.length * 3),
        position: new Float32Array(points.length * 3 + parameters.references.length * 3),
        color: new Float32Array(points.length * 3 + parameters.references.length * 3),
        color_indx: new Float32Array(points.length  + parameters.references.length),
        opacity: new Float32Array(points.length  + parameters.references.length),
        radius: new Float32Array(points.length + parameters.references.length),
        radiusText: new Float32Array(points.length + parameters.references.length),
        radius_indx: new Float32Array(points.length + parameters.references.length),
        visibility: new Float32Array(points.length * 3 + parameters.references.length * 3),
        highlight: new Float32Array(points.length + parameters.references.length)

    };

    fillTheBuffer(null);

    var cloud = new THREE.BufferGeometry();

    cloud.addAttribute('popup', new THREE.BufferAttribute(buffer.popup, 3));
    cloud.addAttribute('position', new THREE.BufferAttribute(buffer.position, 3));
    cloud.addAttribute('radius', new THREE.BufferAttribute(buffer.radius, 1));
    cloud.addAttribute('radius_indx', new THREE.BufferAttribute(buffer.radius_indx, 1));
    cloud.addAttribute('radiusText', new THREE.BufferAttribute(buffer.radiusText, 1));
    cloud.addAttribute('color', new THREE.BufferAttribute(buffer.color, 3));
    cloud.addAttribute('color_indx', new THREE.BufferAttribute(buffer.color_indx, 1));
    cloud.addAttribute('opacity', new THREE.BufferAttribute(buffer.opacity, 1));
    cloud.addAttribute('visibility', new THREE.BufferAttribute(buffer.visibility, 3));
    cloud.addAttribute('hightlight', new THREE.BufferAttribute(buffer.highlight, 1));

    uniforms = {

        textureA: {
            value: new THREE.TextureLoader().load(circularPoint)
        },
        textureB: {
            value: new THREE.TextureLoader().load(rectangularPoint)
        },
        useColor: {
            type: 'f',
            value: 1.0
        },
        orientation: {
            value: new THREE.Vector3()
        },
        direction: {
            value: new THREE.Vector3()
        },
        resolution: {
            value: new THREE.Vector2(window.innerWidth, window.innerHeight)
        },
        size: {
            value: 1
        },
        scale: {
            value: window.innerHeight / 2
        },
        zoom: {
            value: 1.0
        },

    };

    var glsl = new THREE.ShaderMaterial(

        {
            uniforms: uniforms,
            vertexShader: document.getElementById('vertexShader').textContent,
            fragmentShader: document.getElementById('fragmentShader').textContent,
            transparent : true,
            alphaTest: 0.9

        });

    glsl.extensions.fragDepth = true;
    glsl.extensions.drawBuffers = true;

    uniforms = {

        textureA: {
            value: new THREE.TextureLoader().load(circularPoint)
        },
        textureB: {
            value: new THREE.TextureLoader().load(rectangularPoint)
        },
        useColor: {
            type: 'f',
            value: 1.0
        },
        orientation: {
            value: new THREE.Vector3()
        },
        direction: {
            value: new THREE.Vector3()
        },
        resolution: {
            value: new THREE.Vector2(window.innerWidth, window.innerHeight)
        },
        size: {
            value: 1
        },
        scale: {
            value: window.innerHeight / 2
        },
        zoom: {
            value: 1.0
        },

    };

    var glslTarget = new THREE.ShaderMaterial(

        {
            uniforms: uniforms,
            vertexShader: document.getElementById('vertexShaderTarget').textContent,
            fragmentShader: document.getElementById('fragmentShaderTarget').textContent,
            transparent : false

        });

    glslTarget.extensions.fragDepth = true;
    glslTarget.extensions.drawBuffers = true;
    
    pointCloud = new THREE.Points(cloud, glsl);
    pointCloud.dynamic = true;
    pointCloud.frustumCulled = false;
    scene.add(pointCloud);
    
    pointCloudTarget = new THREE.Points(cloud, glslTarget);
    pointCloudTarget.dynamic = true;
    pointCloudTarget.frustumCulled = false;
    pickingScene.add(pointCloudTarget);
    

    if (parameters.centroid) {
        remapValues();
    }

    //GRID HELPER

    var size = 148;
    var divisions = 16;

    var gridHelper = new THREE.GridHelper(size, divisions, 0xDEDEDE, 0xDEDEDE);
    gridHelper.position.x = 74;
    gridHelper.position.z = 74;

    // scene.add(gridHelper);

    //AXES

    var axisMat = new THREE.LineBasicMaterial({
        color: 0x000000
    });

    var coords = [

		[new THREE.Vector3(-198, 0, 0), new THREE.Vector3(198, 0, 0)],
		[new THREE.Vector3(0, -198, 0), new THREE.Vector3(0, 198, 0)],
		[new THREE.Vector3(0, 0, -198), new THREE.Vector3(0, 0, 198)]

	];

    coords.forEach(function (c_) {

        var geometry = new THREE.Geometry();
        geometry.vertices.push(c_[0]);
        geometry.vertices.push(c_[1]);

        var line = new THREE.Line(geometry, axisMat);
        scene.add(line);

    });

    var textMaterial = new THREE.MeshBasicMaterial({
        color: 0x000000
    });

    axesLabels = [];

    //x min max
    axesLabels["xMin"] = {
        l: setRealValue(limits.x.min).toFixed(2),
        f: function (this_) {
            this_.rotateX(-Math.PI / 2);
        },
        offset: new THREE.Vector3(-224, -3, 0),
        v0: null,
        v1: null
    };

    axesLabels["xMax"] = {
        l: setRealValue(limits.x.max).toFixed(2),
        f: function (this_) {
            this_.rotateX(-Math.PI / 2);
        },
        offset: new THREE.Vector3(212, -3, 0),
        v0: null,
        v1: null
    };

    //y min max
    axesLabels["yMin"] = {
        l: setRealValue(limits.y.min).toFixed(2),
        f: function (this_) {
            this_.rotateZ(Math.PI / 2);
        },
        offset: new THREE.Vector3(3, -224, 0),
        v0: null,
        v1: null
    };
    axesLabels["yMax"] = {
        l: setRealValue(limits.y.max).toFixed(2),
        f: function (this_) {
            this_.rotateZ(Math.PI / 2);
        },
        offset: new THREE.Vector3(3, 212, 0),
        v0: null,
        v1: null
    };

    //z min max
    axesLabels["zMin"] = {
        l: setRealValue(limits.z.min).toFixed(2),
        f: function (this_) {
            this_.rotateY(Math.PI / 2);
            this_.rotateX(-Math.PI / 2);
        },
        offset: new THREE.Vector3(0, -3, -224),
        v0: null,
        v1: null
    };
    axesLabels["zMax"] = {
        l: setRealValue(limits.z.max).toFixed(2),
        f: function (this_) {
            this_.rotateY(Math.PI / 2);
            this_.rotateX(-Math.PI / 2);
        },
        offset: new THREE.Vector3(0, -3, 220),
        v0: null,
        v1: null
    };

    for (var key in axesLabels) {


        var a_ = axesLabels[key];
        var ageom = new THREE.TextGeometry(a_.l, {

            font: font_,
            size: 6,
            height: 0.1,
            curveSegments: 12,
            bevelEnabled: false,

        });

        var alabel = new THREE.Mesh(ageom, textMaterial);
        a_.f(alabel);

        if (a_.v0 != null || a_.v1 != null) {
            alabel.position[a_.v0] = 198 - new THREE.Box3().setFromObject(alabel).getSize(new THREE.Vector3())[a_.v1];
        }
        alabel.position.add(a_.offset)
        alabel.name = "helper_" + key;
        alabel.geometry.center();
        scene.add(alabel);

    }

    controls = new THREE.OrbitControls(camera, document.getElementById("scatter3D"));
    controls.enableDamping = true;
    controls.enablePan = true;
    controls.dampingFactor = 0.25;
    controls.screenSpacePanning = parameters.spacePanning;
    controls.minDistance = parameters.zoomLimits.min;
    controls.maxDistance = parameters.zoomLimits.max;

    controls.mouseButtons = { ORBIT: THREE.MOUSE.RIGHT, ZOOM: THREE.MOUSE.MIDDLE, PAN: THREE.MOUSE.LEFT };
    
    raycaster = new THREE.Raycaster();
    mouse = new THREE.Vector2();
    innerDimensions = new THREE.Vector2();
    
    inverseMatrix = new THREE.Matrix4();
    ray = new THREE.Ray();

    document.addEventListener('keydown', function(event){
        if(event.keyCode == 16) { shift = true; }
        } );
        
        document.addEventListener('keyup', function(event){
        shift = false;
        } );
    
    document.addEventListener('mousemove', function (e_) {

            camera.clearViewOffset();

            mouse.x = (e_.clientX / window.innerWidth) * 2 - 1;
            mouse.y = -(e_.clientY / window.innerHeight) * 2 + 1;

            renderer.render(pickingScene, camera, pickingTexture);
            var pixelBuffer = new Uint8Array(4);
            renderer.readRenderTargetPixels(
            pickingTexture, e_.clientX * window.devicePixelRatio, pickingTexture.height - e_.clientY * window.devicePixelRatio,
            1, 1, pixelBuffer);
        
            reset_id = (pixelBuffer[0] << 16) | (pixelBuffer[1] << 8) | (pixelBuffer[2]);

            if(reset_id != 0 && reset_id <= points.length + parameters.references.length){
                
                if(last_id != reset_id){
                    
                    buffer.highlight[last_id] = 0.0;
                    buffer.highlight[reset_id] = 1.0;
                    last_id = reset_id;
                    pointCloud.geometry.addAttribute('highlight', new THREE.BufferAttribute(buffer.highlight, 1));
                    
                }

            }else{
                
                last_id = 0;
                for(var key in points) { buffer.highlight[key] = 0.0; }
                for(var key in parameters.references) { buffer.highlight[points.length + Number(key)] = 0.0; }
                pointCloud.geometry.addAttribute('highlight', new THREE.BufferAttribute(buffer.highlight, 1));
                
            }

    });

    document.addEventListener("click", function (e_) {

         if (clickable && last_id != 0 && last_id < points.length && last_id != undefined && last_id != null && reset_id == last_id) {

                ethno.popup.forEach(function (p_) {

                    var point = points[last_id];
                    var indices = p_.index.split(".");
                    document.getElementById(indices[1]).innerHTML = "<b>" + ethno.checkbox.dict[indices[1]] + "</b><br>" + point[indices[0]][indices[1]];

                });

                maximizePopup();
	     
                div.style("left", function () {
                        return window.innerWidth / 2 - 200;
                    })
                    .style("top", function () {
                        return window.innerHeight / 2 - 150
                    })
                    .style("pointer-events", "all");
                div.style("display", "grid");
                divState = true;
                clickable = false;

            }
	
            if (clickable && last_id != 0 && last_id >= points.length && last_id <= (points.length + parameters.references.length) && last_id != undefined && last_id != null && reset_id == last_id) {

                var indx = last_id - points.length;
                
                minimizePopup();

                document.getElementById("culture").innerHTML = "<b>" + parameters.references[indx].popup + "</b>" 
                
                div.style("left", function () {
                        return window.innerWidth / 2 - 200;
                    })
                    .style("top", function () {
                        return window.innerHeight / 2 - 150
                    })
                    .style("pointer-events", "all");
                div.style("display", "grid");
                divState = true;
                clickable = false;

            }
    });

    animate();


}

function setRealValue(v_) { return v_; }

function animate() {

    var scale = 1.0;

    if (!parameters.scalable && !parameters.centroid) {
        scale = 768.0 / new THREE.Vector3(0, 0, 0).subVectors(new THREE.Vector3(0, 0, 0), camera.position).length();
    }

    labels = [

        new THREE.Vector3(184.0, 1E-5, 1E-5),
        new THREE.Vector3(1E-5, 184.0, 1E-5),
        new THREE.Vector3(1E-5, 1E-5, 184.0)
        
    ];
    
    axes = [

		[new THREE.Vector3(146.0 / scale, 1E-5, 1E-5), new THREE.Vector3(237.0 / scale, 1E-5, 1E-5)],
		[new THREE.Vector3(1E-5, 136.0 / scale, 1E-5), new THREE.Vector3(1E-5, 247.0 / scale, 1E-5)],
		[new THREE.Vector3(1E-5, 1E-5, 136.0 / scale), new THREE.Vector3(1E-5, 1E-5, 237.0 / scale)]

	];

    values = [

		new THREE.Vector3(-212.0 / scale, 1E-5, 1E-5), //negative X
		new THREE.Vector3(212.0 / scale, 1E-5, 1E-5),  //positive X
		new THREE.Vector3(1E-5, -212.0 / scale, 1E-5), //negative Y
		new THREE.Vector3(1E-5, 212.0 / scale, 1E-5),  //positive Y
		new THREE.Vector3(1E-5, 1E-5, -222.0 / scale), //negative Z
		new THREE.Vector3(1E-5, 1E-5, 222.0 / scale)   //positive Z

	];

    grids = [];

    for (var xz = 0; xz <= parameters.gridSize; xz += parameters.gridSize / parameters.gridMarkup) {

        grids.push([new THREE.Vector3(xz / scale, 1E-5, 1E-5), new THREE.Vector3(xz / scale, 1E-5, parameters.gridSize / scale)]);

        grids.push([new THREE.Vector3(1E-5, 1E-5, xz / scale), new THREE.Vector3(parameters.gridSize / scale, 1E-5, xz / scale)]);

    }

    if (parameters.centroid) {
        axes = offsetXYZ(axes);
        values = offsetXYZ(values);
        grids = offsetXYZ(grids);
    };

    
    var projectedLabels = svg.selectAll("text.Labels").data(labels);
    
    d3.select("#xPath").attr("d", setPath(axes[0], "x", camera.rotation.y));
    d3.select("#yPath").attr("d", setPath(axes[1], "y", null));
    d3.select("#zPath").attr("d", setPath(axes[2], "z", camera.rotation.y));
    
    if(camera.rotation.y > 0 && camera.rotation.y < 3.14){
    d3.select(".zLabel").attr("startOffset", "66%");    
    }else{
    d3.select(".zLabel").attr("startOffset", "33%");
    }
    
    if(camera.rotation.y < 0 ){
    d3.select(".xLabel").attr("startOffset", "66%");    
    }else{
    d3.select(".xLabel").attr("startOffset", "33%");
    }

    pointCloud.material.uniforms.zoom.value = controls.target.distanceTo(controls.object.position) / 768;

    for (var key in axesLabels) {

        if (key.length > 1) {
            var obj = scene.getObjectByName("helper_" + key);
            obj.lookAt(camera.position);
        }

    }

    controls.update();
    requestAnimationFrame(animate);
    if (parameters.statsOn) {
        stats.update();
    }

    renderer.render(scene, camera);

}

function checkCulturesVisibility() {


    for (var i in points) {

        var culture = ethno.checkbox.cultures[points[i].id_nhs];

        if (culture.region == "undefined" || culture.sub == "undefined") {

            if (document.getElementsByName("check_undefined")[0].value == -1) {

                buffer.visibility[i * 3 + 2] = 0;

            } else {

                buffer.visibility[i * 3 + 2] = 1;

            }

        } else {

            if (document.getElementsByName("check_" + culture.sub.replace(" ", "_"))[0].value == -1) {

                buffer.visibility[i * 3 + 2] = 0;

            } else {

                buffer.visibility[i * 3 + 2] = 1;

            }

        }
    }

    pointCloud.geometry.attributes.visibility.needsUpdate = true;

}

 function minimizePopup(){

     div.style("grid-template-areas", "'clr clr clr clr clr x' 'clt clt clt clt clt clt'");
     div.style("height", "auto");
     parameters.popup.forEach(function(d_){ div.select("#" + d_).style("display", "none"); });
     
 }

 function maximizePopup(){
     
     div.style("grid-template-areas", "'clr clr clr clr clr x' 'clt clt clt clt clt clt' 'txt txt txt txt txt txt' 'lyr lyr lyr lyr lyr lyr' 'cit cit cit cit cit cit' 'ocm ocm ocm ocm ocm omc'");
     div.style("height", "300px");
     parameters.popup.forEach(function(d_){ div.select("#" + d_).style("display", "inline"); })
     
 }

function fillTheBuffer(out_) {

    var count = 1;
    for (var i in points) {

        buffer.highlight[i] = 0.0;
        
        buffer.id[i] = points[i].indx;

        var indx = new THREE.Color().setHex(count);
        count++;
        
        buffer.popup[i * 3] = indx.r;
        buffer.popup[i * 3 + 1] = indx.g;
        buffer.popup[i * 3 + 2] = indx.b;

        buffer.position[i * 3] = points[i][ethno.x] * parameters.ratio;
        buffer.position[i * 3 + 1] = points[i][ethno.y] * parameters.ratio;
        buffer.position[i * 3 + 2] = points[i][ethno.z] * parameters.ratio;

        if (parameters.radius.index == "na") {
	    
            buffer.radius[i] = parameters.radius.dict[0].r * parameters.radiusGauge;
            buffer.radius_indx[i] = 0;
        } else {

            var indices = parameters.radius.index.split(".");
            buffer.radius[i] = parameters.radius.f(points[i][indices[0]][indices[1]], "r") * parameters.radiusGauge;
            buffer.radius_indx[i] = parameters.radius.f(points[i][indices[0]][indices[1]], "i");
            buffer.radiusText[i] = parameters.radius.f(points[i][indices[0]][indices[1]], "t");

        }

        var tmpc = "#DEDEDE";
        var color_indx = -1;

        if (!parameters.color.index.includes('.')) {

            if (parameters.color.f(points[i][parameters.color.index]) != -1) {

                tmpc = parameters.color.colors[parameters.color.f(points[i][parameters.color.index])];
                color_indx = parameters.color.f(points[i][parameters.color.index]);

            }

        } else {

            if (parameters.color.f(points[i][parameters.color.index]) != -1) {

                var indices = parameters.color.index.split(".");

                if (parameters.color.f(points[i][indices[0]][indices[1]]) != -1) {

                    tmpc = parameters.color.colors[parameters.color.f(points[i][indices[0]][indices[1]])];
                    color_indx = parameters.color.f(points[i][indices[0]][indices[1]]);

                }

            }

        }

        if (parameters.color.index == "na") {

            tmpc = parameters.na_color;
            color_indx = 0;

        }

        var rgb = hexToRGB(tmpc);
        if(JSON.parse(points[i].highlighted)) { rgb = convertRGB2Floats(d3.rgb(tmpc).darker(parameters.highlightedDarker).toString()); }

        buffer.color[i * 3] = rgb.r;
        buffer.color[i * 3 + 1] = rgb.g;
        buffer.color[i * 3 + 2] = rgb.b;

        buffer.color_indx[i] = color_indx;

        JSON.parse(points[i].highlighted) ? buffer.opacity[i] = parameters.highlightedAlpha : buffer.opacity[i] = parameters.defaultAlpha;
	
        if (out_ == "color") {

            buffer.visibility[i * 3] = 1.0;

        } else if (out_ == "radius") {

            buffer.visibility[i * 3 + 1] = 1.0;

        } else {

            buffer.visibility[i * 3] = 1.0;
            buffer.visibility[i * 3 + 1] = 1.0;
            buffer.visibility[i * 3 + 2] = 1.0;

        }
    	
    }

    var n = points.length;
    var n3 = points.length * 3;

    for (var i in parameters.references) {
        
        var fi = Number(i);
        
        buffer.highlight[n + fi] = 0.0;
        buffer.id[n + fi] = n + fi;

        var indx = new THREE.Color().setHex(count);
        count++;
        
        buffer.popup[n3 + fi * 3] = indx.r;
        buffer.popup[n3 + fi * 3 + 1] = indx.g;
        buffer.popup[n3 + fi * 3 + 2] = indx.b;
        
        buffer.position[n3 + fi * 3] = parameters.references[fi].score_1 * parameters.ratio;
        buffer.position[n3 + fi * 3 + 1] = parameters.references[fi].score_2 * parameters.ratio;
        buffer.position[n3 + fi * 3 + 2] = parameters.references[fi].score_3 * parameters.ratio;

        buffer.radius[n + fi] = parameters.referenceDimensions.r * parameters.radiusGauge;
        buffer.radius_indx[n + fi] = 0;
        buffer.radiusText[n + fi] = 2.0;

        var rgb = convertRGB2Floats(d3.rgb(parameters.color.colors[fi]).darker(parameters.referenceDarkerRatio).toString());

        buffer.color[n3 + fi * 3] = rgb.r;
        buffer.color[n3 + fi * 3 + 1] = rgb.g;
        buffer.color[n3 + fi * 3 + 2] = rgb.b;

        buffer.color_indx[n + fi] = 0;
        buffer.opacity[n + fi] = 0.8;
        
        buffer.visibility[n3 + fi * 3] = 1.0;
        buffer.visibility[n3 + fi * 3 + 1] = 1.0;
        buffer.visibility[n3 + fi * 3 + 2] = 1.0;
  
    }

}

function filterPoints(){

    parameters.filtered = !parameters.filtered;
    
    if(parameters.filtered){
        
        for(var i = 1; i < points.length; i++){

             if(!JSON.parse(points[i].highlighted)) { 
                 
                 buffer.opacity[i] = parameters.defaultAlpha2;
                 buffer.visibility[i * 3] = 0.0;
                 buffer.visibility[i * 3 + 1] = 0.0;
                 buffer.visibility[i * 3 + 2] = 0.0;

             }
            

             document.getElementById("filter").value = "filter on";
            
        } 
        
        
    }else{
                         
        for(var i = 1; i < points.length; i++){
            
             if(!JSON.parse(points[i].highlighted)) { 
                 
                 buffer.opacity[i] = parameters.defaultAlpha; 
                 buffer.visibility[i * 3] = 1.0;
                 buffer.visibility[i * 3 + 1] = 1.0;
                 buffer.visibility[i * 3 + 2] = 1.0;
             
             }

             document.getElementById("filter").value = "filter off";
            
        } 

    }
              
    pointCloud.geometry.attributes.opacity.needsUpdate = true;
    pointCloud.geometry.attributes.visibility.needsUpdate = true;
    
}

function convertRGB2Floats(color_){
    
    color_ = color_.replace("rgb(", "").replace(")", "");
    var rgb = color_.split(",");
    for(i in rgb) { rgb[i] = Number(rgb[i].trim()) / 255; }
    return {r: rgb[0], g: rgb[1], b: rgb[2] };

}

function componentToHex(c_) {
    
  var hex = c_.toString(16);
  return hex.length == 1 ? "0" + hex : hex;
    
}

function rgbToHex(rgb_) { return "#" + componentToHex(rgb_[0]) + componentToHex(rgb_[1]) + componentToHex(b_); }

function setCulturesPopup(db_) {

    for (var key in points){ 

        ethno.checkbox.regions[points[key]["NHSEthnography_Metadata"].id_hraf] = {region: points[key]["NHSEthnography_Metadata"].hraf_region, sub: points[key]["NHSEthnography_Metadata"].hraf_subregion}

        ethno.checkbox.cultures[points[key]["NHSCultures_Metadata"].id_nhs] =  {culture: points[key]["NHSCultures_Metadata"].culture, id_hraf: points[key]["NHSEthnography_Metadata"].id_hraf, region: points[key]["NHSEthnography_Metadata"].hraf_region, sub: points[key]["NHSEthnography_Metadata"].hraf_subregion };
        
        if(ethno.checkbox.index[points[key]["NHSEthnography_Metadata"].hraf_region] == undefined){
            
            ethno.checkbox.index[points[key]["NHSEthnography_Metadata"].hraf_region] = [];
            
            if(ethno.checkbox.index[points[key]["NHSEthnography_Metadata"].hraf_region][points[key]["NHSEthnography_Metadata"].hraf_subregion] == undefined){
                
                ethno.checkbox.index[points[key]["NHSEthnography_Metadata"].hraf_region][points[key]["NHSEthnography_Metadata"].hraf_subregion] = [];
                
                ethno.checkbox.index[points[key]["NHSEthnography_Metadata"].hraf_region][points[key]["NHSEthnography_Metadata"].hraf_subregion].push({culture: points[key]["NHSCultures_Metadata"].culture, id_hraf: points[key]["NHSEthnography_Metadata"].id_hraf, region: points[key]["NHSEthnography_Metadata"].hraf_region, sub: points[key]["NHSEthnography_Metadata"].hraf_subregion });
                
                
            }else{
                
                if(!isCultureAlreadyInList(ethno.checkbox.index[points[key]["NHSEthnography_Metadata"].hraf_region][points[key]["NHSEthnography_Metadata"].hraf_subregion], points[key]["NHSCultures_Metadata"].culture)){
                    
                    ethno.checkbox.index[points[key]["NHSEthnography_Metadata"].hraf_region][points[key]["NHSEthnography_Metadata"].hraf_subregion].push({culture: points[key]["NHSCultures_Metadata"].culture, id_hraf: points[key]["NHSEthnography_Metadata"].id_hraf, region: points[key]["NHSEthnography_Metadata"].hraf_region, sub: points[key]["NHSEthnography_Metadata"].hraf_subregion });
    
                }
    
            }
              
        }else{
            
            if(ethno.checkbox.index[points[key]["NHSEthnography_Metadata"].hraf_region][points[key]["NHSEthnography_Metadata"].hraf_subregion] == undefined){
                
                ethno.checkbox.index[points[key]["NHSEthnography_Metadata"].hraf_region][points[key]["NHSEthnography_Metadata"].hraf_subregion] = [];
                
                ethno.checkbox.index[points[key]["NHSEthnography_Metadata"].hraf_region][points[key]["NHSEthnography_Metadata"].hraf_subregion].push({culture: points[key]["NHSCultures_Metadata"].culture, id_hraf: points[key]["NHSEthnography_Metadata"].id_hraf, region: points[key]["NHSEthnography_Metadata"].hraf_region, sub: points[key]["NHSEthnography_Metadata"].hraf_subregion });
 
            }else{
                
                if(!isCultureAlreadyInList(ethno.checkbox.index[points[key]["NHSEthnography_Metadata"].hraf_region][points[key]["NHSEthnography_Metadata"].hraf_subregion], points[key]["NHSCultures_Metadata"].culture)){
                    
                    ethno.checkbox.index[points[key]["NHSEthnography_Metadata"].hraf_region][points[key]["NHSEthnography_Metadata"].hraf_subregion].push({culture: points[key]["NHSCultures_Metadata"].culture, id_hraf: points[key]["NHSEthnography_Metadata"].id_hraf, region: points[key]["NHSEthnography_Metadata"].hraf_region, sub: points[key]["NHSEthnography_Metadata"].hraf_subregion });
                                
                }
                
            }
  
        }
                
    } 

    buildCultuesPopup();

}

function buildCultuesPopup() {

    //button
    var btn = document.createElement("BUTTON");
    var t = document.createTextNode("select all");

    btn.appendChild(t);
    btn.setAttribute("onClick", "javascript: selectAll();");

    document.getElementById("selectAll").appendChild(btn);

    var btn = document.createElement("BUTTON");
    var t = document.createTextNode("select all");

    var str = "";
    var und = "";

    for (var key in ethno.checkbox.index) {

        if (key != "undefined") {

            var el0 = [];

            for (var key2 in ethno.checkbox.index[key]) {

                el0 += "check_" + key2.replace(" ", "_") + ",";

                ethno.checkbox.index[key][key2].forEach(function (tmp_) {

                    el0 += "check_" + tmp_.culture.replace(" ", "_") + ",";

                });


            }

            el0 = el0.replace(/.$/, "");
            var str = "";
            str += "<ul><h3><input type=\"checkbox\" class=\"checkbox\" id=\"" + el0 + "\"name=\"check_" + key.replace(" ", "_") + "\" value=\"1\" onclick=\"toggleCheckbox(this)\" checked>" + key + "</h3><ul>";

            for (var key2 in ethno.checkbox.index[key]) {

                var el1 = "";

                ethno.checkbox.index[key][key2].forEach(function (tmp_) {

                    el1 += "check_" + tmp_.culture.replace(" ", "_") + ",";

                })

                el1 = el1.replace(/.$/, "");

                str += "<li><input class=\"checkbox\" type=\"checkbox\" name=\"check_" + key2.replace(" ", "_") + "\" value=\"1\" onclick=\"toggleCheckbox(this)\" id=\"" + el1 + "\" style=\"display:none;\" checked><ul>";

                ethno.checkbox.index[key][key2].forEach(function (c_) {

                    str += "<li><input id=\"\" class=\"checkbox\" type=\"checkbox\" name=\"check_" + c_.culture.replace(" ", "_") + "\" value=\"1\" onclick=\"toggleCheckbox(this)\" checked>" + c_.culture + "</li>";

                })

                str += "</ul></li>";

            }

            str += "</ul>";
            document.getElementById(key.replace(/ /g, "_")).innerHTML = str;

        } else {

            und += "<li><h3><input type=\"checkbox\" class=\"checkbox\" name=\"check_" + key.replace(" ", "_") + "\" value=\"1\" checked><span style=\"color:darkred;\">" + key + "</font></h3><ul>";

            und += "</ul></li>";

        }
    }

    document.getElementById("Middle_East").innerHTML += und;

}

function isCultureAlreadyInList(array_, culture_){
    
    var found = false;
    array_.forEach(function(d_){ if(d_.culture == culture_) { found = true; } })
    return found;
    
}

function toggleCheckbox(e_) {

    e_.value *= -1;

    if (e_.id != "") {


        var nm = e_.id;
        var children = nm.split(",");

        for (var i = 0; i < children.length; i++) {

            document.getElementsByName(children[i])[0].value = e_.value;
            document.getElementsByName(children[i])[0].checked = e_.checked;

        }

    }

    checkCulturesVisibility();

}

function selectAll() {


    var cb = document.getElementsByClassName("checkbox");
    for (var i = 0, n = cb.length; i < n; i++) {
        cb[i].checked = true;
        cb[i].value = 1;
    }

    checkCulturesVisibility();

}

function closePopup() {

    document.getElementById("culturesPopup").style.display = "none";
    clickable = true;
    rendering = true;

}

function showInfoBox() {

    document.getElementById("infoPopup").style.display = "grid";
    clickable = false;
    rendering = false;

}

function closeInfo() {

    document.getElementById("infoPopup").style.display = "none";
    clickable = true;
    rendering = true;

}

function restructurePoints(points_, db_) {

    points_.forEach(function (p_) {

        Object.keys(ethno.indices).forEach(function (i_) {

            if (!ethno.indices[i_].includes('.')) {

                p_[i_] = db_[i_.toString()][p_[ethno.indices[i_.toString()]]];

            } else {

                var indices = ethno.indices[i_].split(".");
                p_[i_] = db_[i_.toString()][p_[indices[0]][indices[1]]];

            }

        })

    });

}

function splitToMultiLines(str_, width_, brk_, cut_) {

    brk_ = brk_ || '\n';
    width_ = width_ || 75;
    cut_ = cut_ || false;
    if (!str_) {
        return str_;
    }
    var regex = '.{1,' + width_ + '}(\\s|$)' + (cut_ ? '|.{' + width_ + '}|.+$' : '|\\S+?(\\s|$)');
    return str_.match(RegExp(regex, 'g')).join(brk_);

}

function makeEvenRadiuses(obj_) {

    obj_.forEach(function (d_) {

        d_.dict.forEach(function (k_, i_) {
            k_.r = parameters.radiusScale(i_ + 1);
        });
    });

}

function changeColors() {

    parameters.color = getElementByName(ethno.color, document.getElementById("colorSelector").value);
    renderLegend();

    fillTheBuffer("color");

    pointCloud.geometry.attributes.color.needsUpdate = true;
    pointCloud.geometry.attributes.radius.needsUpdate = true;
    pointCloud.geometry.attributes.visibility.needsUpdate = true;
    pointCloud.geometry.attributes.color_indx.needsUpdate = true;

}

function changeRadiuses() {

    parameters.radius = getElementByName(ethno.radius, document.getElementById("radiusSelector").value);
    renderLegend();

    fillTheBuffer("radius");

    pointCloud.geometry.attributes.color.needsUpdate = true;
    pointCloud.geometry.attributes.radius.needsUpdate = true;
    pointCloud.geometry.attributes.radiusText.needsUpdate = true;
    pointCloud.geometry.attributes.visibility.needsUpdate = true;
    pointCloud.geometry.attributes.radius_indx.needsUpdate = true;

}

function setPath(v_, axis_,  y_) {

    var v0 = valueValidation(toScreenXY(v_[0].x, v_[0].y, v_[0].z, camera, null));
    var v1 = valueValidation(toScreenXY(v_[1].x, v_[1].y, v_[1].z, camera, null));

    if(axis_ == "x"){
        
        if(y_ < 0){
            
        var tmp = v0;
        v0 = v1;
        v1 = tmp;
            
        }
        
        
    }
    if(axis_ == "z"){
        
        if(y_ > 0 && y_ < 3.14){
            
        var tmp = v0;
        v0 = v1;
        v1 = tmp;
            
        }
        
    }

    return "M" + v0.x + "," + v0.y + " L" + v1.x + "," + v1.y;

}

function offsetXYZ(vertices_) {

    vertices_.forEach(function (v_) {

        if (v_.constructor != Array) {
            v_.x -= 98;
            v_.z -= 98;
        } else {
            offsetXYZ(v_);
        }

    });

    return vertices_;

}

function toScreenXY(x_, y_, z_, camera_, offset_) {

    var p = new THREE.Vector3(x_, y_, z_);
    if (offset_ != null && parameters.mousemode == "pan") {
        p.add(offset_);
    }
    var vector = p.project(camera_);

    vector.x = (vector.x + 1) / 2 * w;
    vector.y = -(vector.y - 1) / 2 * h;

    return {
        x: vector.x,
        y: vector.y
    };

}

function isPointOnScreen(d_, p_) {

    var tmp = new THREE.Vector3(d_.x + p_.x, d_.y + p_.y, d_.z + p_.z);

    var xy = toScreenXY(tmp.x, tmp.y, tmp.z, camera);

    if (!xy.x.between([0, w])) {
        return false;
    }
    if (!xy.y.between([0, h])) {
        return false;
    }

    return true;

}

function valueValidation(v_) {

    if (v_ == Infinity || v_ == -Infinity) {
        return 1E-5
    }
    return v_ || 1E-5;

};

function onWindowResize() {

    w = window.innerWidth, h = window.innerHeight;

    svg.attr("width", w).attr("height", h);
    legendBox.attr("transform", function () {
        return "translate(" + (document.getElementById("ui").offsetLeft) + "," + (document.getElementById("ui").offsetTop + document.getElementById("ui").offsetHeight + 16) + ")";
    });

    renderer.setPixelRatio(window.devicePixelRatio);
    renderer.setSize(w, h);

    camera.aspect = w / h;
    camera.updateProjectionMatrix();

    pickingTexture.width = w;
    pickingTexture.height = h;
    


}

function switchControls(key_) {

    var cntrl = document.getElementById("cntrl");

    if (key_ == "ui") {

        if (cntrl.value == "Rotate") {

            cntrl.value = cntrl.name = "Drag";
            parameters.drag = true;
            controls.mouseButtons = { ORBIT: THREE.MOUSE.LEFT, ZOOM: THREE.MOUSE.MIDDLE, PAN: THREE.MOUSE.RIGHT }; 

	    
        } else {

            cntrl.value = cntrl.name = "Rotate";
            parameters.drag = false;
            controls.mouseButtons = { ORBIT: THREE.MOUSE.RIGHT, ZOOM: THREE.MOUSE.MIDDLE, PAN: THREE.MOUSE.LEFT };

        }
	
    }

    else {

        if (key_ == "shiftDown") {

            cntrl.value = cntrl.name = "Drag";
            controls.mouseButtons = { ORBIT: THREE.MOUSE.LEFT, ZOOM: THREE.MOUSE.MIDDLE, PAN: THREE.MOUSE.RIGHT };
            parameters.drag = true;
            parameters.shift = true;


        } else {

            cntrl.value = cntrl.name = "Rotate";
            controls.mouseButtons = { ORBIT: THREE.MOUSE.RIGHT, ZOOM: THREE.MOUSE.MIDDLE, PAN: THREE.MOUSE.LEFT };
            parameters.drag = false;
            parameters.shift = false;

        }


    }

}

function getElementByName(array_, name_) {

    if (array_.filter(e_ => e_.index === name_).length > 0) {
        return array_.filter(e_ => e_.index === name_)[0];
    }

}

function getIndexByNHS(index_) {


    if (points.filter(e_ => e_.id_nhs === index_).length > 0) {
        return points.filter(e_ => e_.id_nhs === index_)[0].indx;
    }
    console.log("data broken");
    return false;

}

function setOffset(d_) {

    out = 0;

    for (var i = 0; i < d_; i++) {

        out += (15 + i * 5) * 1.2;
    }

    return out;
}

function opticalCompensation(d_) {
    return (15 + d_ * 5) * 0.35;
}

function backToOrigin() {

    pointCloud.position.set(0, 0, 0);
    camera.position.set(256, 512, 512);

}

function closeTooltip() {

    div.style("display", "none");
    last_id = null;
    divState = false;
    clickable = true;

}

function nhsExists(index_) {

    if (points.filter(e_ => e_.id_nhs === index_).length > 0) {
        return true;
    }
    return false;

}

function getSongFunction(n_) {

    return 0;

}

function exists(index_) {

    if (points[index_] != undefined) return true;
    return false;

}

function checkDictionary(name_) {

    if (ethno.checkbox.dictionary[name_] != undefined) {
        return ethno.checkbox.dictionary[name_];
    } else {
        return name_;
    }

}

function ifDefined(hraf_) {

    if (hraf_ != '.') {
        return hraf_;
    } else {
        return "none";
    }

}

function hexToRGB(hex_) {

    var result = /^#?([a-f\d]{2})([a-f\d]{2})([a-f\d]{2})$/i.exec(hex_);
    return result ? {

        r: parseInt(result[1], 16) / 255,
        g: parseInt(result[2], 16) / 255,
        b: parseInt(result[3], 16) / 255

    } : null;

}

Number.prototype.between = function (domain_) {

    var min = Math.min.apply(Math, domain_);
    var max = Math.max.apply(Math, domain_);

    return this >= min && this <= max;

};

d3.selection.prototype.moveToFront = function () {

    return this.each(function () {
        this.parentNode.appendChild(this);
    });

};
