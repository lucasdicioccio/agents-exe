(function() {
  const go = () => {
    var chartDom = document.getElementById('histogram');
    var myChart = echarts.init(chartDom);
    var option;

    window.addEventListener('resize', () => myChart.resize());
    
    const jsonSrc = document.getElementsByName('ks:article_json')[0].content;
    
    const zeroes = (n) => {
      const ary = new Array(n);
      for (idx=0; idx<n; idx++) {
        ary[idx] = 0;
      }
      return ary;
    }
    
    myChart.showLoading();
    fetch(jsonSrc)
    .then((response) => response.json())
    .then((articlejson) => {
      myChart.hideLoading();
      const slitems = articlejson.skyline.skylineItems;
    
      const nSeries = slitems.filter((item) => item.tag == 'HeaderMark').length + 2; // one for 'previous cumulative series' and one for the 'incipit' section
      const newSeries = (name) => { return { name: name, type: 'bar', stack: 'one', data: zeroes(nSeries) } };
      let series = [newSeries('previous'), newSeries('incipit')];
      const offsetSeries = series[0];
    
      let cumulativeSum = 0;
      let seriesIdx = 1;
      let currentSeries = series[1];
      slitems.forEach((item) => {
        if (item.tag == 'HeaderMark') {
          const title = item.contents[0];
          const heading = item.contents[1].reduce((c,x) => `${x}.${c}`, '');
          const s = newSeries(`${heading} ${title}`);
          series.push(s);
          seriesIdx += 1;
          currentSeries = s;
          offsetSeries.data[seriesIdx] = cumulativeSum;
        }
        if (item.tag == 'TextualMark') {
          const val = item.contents[0];
          cumulativeSum += val;
          currentSeries.data[seriesIdx] += val;
        }
      });
    
      offsetSeries.itemStyle = {'borderColor': 'transparent', color: 'transparent'};
      offsetSeries.emphasis = {'itemStyle':  {'borderColor': 'transparent', color: 'transparent'}};
    
      option = {
        title: {
          text: 'article histogram',
          subtext: 'words per paragraph',
          top: 'bottom',
          left: 'right'
        },
        tooltip: {
          trigger: 'axis',
          axisPointer: {type: 'shadow'},
        },
        xAxis: {
            name: 'cmark heading',
            data: series.map((_,i) => i),
            silent: true,
        },
        yAxis: {
          name: 'total words',
          type: 'value'
        },
        series: series,
      };
      myChart.setOption(option);
    });
    
    option && myChart.setOption(option);
  };
  document.addEventListener("DOMContentLoaded", go);
})();

