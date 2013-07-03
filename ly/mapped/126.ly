\version "2.14.2"
\include "util.ly"
\header{ tagline = ""}
\paper {
  print-all-headers = ##t
  ragged-right = ##f
  %print-all-headers = ##t
  paper-height = 11\in
  paper-width = 8.5\in
  indent = 0\in
  %system-system-spacing = #'((basic-distance . 10) (padding . 0))
  system-system-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -1)
       (stretchability . 100))
  ragged-last-bottom = ##f
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 1\in
  outer-margin = 0.75\in
  top-margin = 0.26\in
  bottom-margin = 0.25\in
  first-page-number = #126
  print-first-page-number = ##t
  headerLine = ""
  oddHeaderMarkup = \markup\fill-line{
     \override #'(font-name . "Garamond Premier Pro")\abs-fontsize #12.5
     \combine 
        \fill-line{"" \on-the-fly #print-page-number-check-first
        \oldStylePageNum""
        }
        \fill-line{\headerLine}
  }
  evenHeaderMarkup = \markup {
     \override #'(font-name . "Garamond Premier Pro")\abs-fontsize #12.5
     \combine
        \on-the-fly #print-page-number-check-first
        \oldStylePageNum""
        \fill-line{\headerLine}
  }
}
#(set-global-staff-size 18) \paper{ #(define fonts (make-pango-font-tree "Garamond Premier Pro" "Garamond Premier Pro" "Garamond Premier Pro" (/ 18 20))) }
global = {
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
  \tieDashed
}

sopMusic = \relative c' {
  \partial 4
  \repeat volta 2 {
    b8\rest b8 |
    e4 e e b8 cis |
    b4. a8 gis4 b8\rest a8 |
    b4 b8~ b cis4 b8[ a] |
    
    gis4( fis) b\rest b |
    e8 e e e gis4 fis8 e |
    dis cis e cis b4 b8~ b |
    
    %page2
    e4 dis8~ dis cis4. b8 |
    b2 b4\rest r |
    r2 r4 gis8 ais |
    
    b4 ais gis r |
    r2 r4 ais8 b |
    cis4 b ais r |
    
    r2 r4 gis8 ais |
    b4 ais gis fis8~ fis |
    gis b ais gis ais cis b ais |
    
    %page3
    b4^\markup\italic"rall." b, b \oneVoice b'\fermata \bar"||"
    b b b cis8 dis |
    e4 dis cis b\cresc |
    
    cis4\~ cis cis dis8 e |
    fis4 e dis cis |
    b8~ b b4 b cis8 dis |
    
    e4 dis cis b8~ b |
    cis8\f e dis cis b a gis fis |
  }
  \alternative {
    {
      gis4 e e b'4\rest\fermata
    }
    {
      gis e e e' |
    }
  }
  
  fis2 dis |
  
  e b4\rest e4 |
  fis2 dis |
  e b4\rest e |
  fis1 |
  <b dis,>\fermata |
  e,2.
}
sopWords = \lyricmode {
  \set stanza = #"1. "
  \set ignoreMelismata = ##t
	A Brit -- ish tar is a soar -- ing soul,
  As free as a moun -- tain _ bird; _
  His en -- er -- get -- ic fist
  Should be rea -- dy to re -- sist
  A __ _ dic -- _ ta -- _ tor -- ial word.
  
  \set associatedVoice = "altos" 
  His nose should \set associatedVoice = "sopranos" pant,
  \set associatedVoice = "sopranos"
  And his lip should \set associatedVoice = "altos" curl,
  \set associatedVoice = "altos"
  His cheeks should \set associatedVoice = "sopranos" flame
  \set associatedVoice = "sopranos"
  And his brow should \set associatedVoice = "altos" furl,
  \set associatedVoice = "altos"
  His bo -- som should \set associatedVoice = "sopranos" heave,
  \set associatedVoice = "sopranos"
  And his heart should glow,
  And his fist be ev -- er rea -- dy
  For a knock down blow.
  
  
  His nose should pant,
  And his lip should curl,
  His cheeks should flame
  And his brow should furl,
  His bo -- som should heave,
  And his heart should glow,
  And his fist be ev -- er rea -- dy
  For a knock down blow.
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  \set ignoreMelismata = ##t
  His eyes should flash with an in -- born fire,
  His brow with _ scorn be _ wrung; _
  He nev -- er should bow down
  To a dom -- i -- neer -- ing frown,
  Or the tang _ of a ty -- rant tongue.
  
  
  \set associatedVoice = "altos"
  His foot should \set associatedVoice = "sopranos" stamp,
  \set associatedVoice = "sopranos"
  And his throat should \set associatedVoice = "altos" growl,
  \set associatedVoice = "altos"
  His hair should \set associatedVoice = "sopranos" twirl,
  \set associatedVoice = "sopranos"
  And his face should \set associatedVoice = "altos" scowl,
  \set associatedVoice = "altos"
  His eyes _ should \set associatedVoice = "sopranos" flash
  \set associatedVoice = "sopranos"
  And his breast pro -- trude.
  And _ this should be his cus -- tom -- a -- ry at -- ti -- tude.
  
  
  His foot should stamp,
  And his throat should growl,
  His hair should twirl,
  And his face should scowl,
  His eyes _ should flash
  And his breast pro -- trude.
  And _ this should be his cus -- tom -- a -- ry "" "" ""
  at -- ti -- tude,
  his at -- ti -- tude, his at -- ti -- tude, his at -- ti -- tude.
}

sopWordsIII = \lyricmode {
  \set stanza = #"3. "
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
}

altoMusic = \relative c' {
  \repeat volta 2 {
    s8 b8 |
    gis4 gis gis gis8 gis |
    fis4 dis e s8 e |
    e4 e8~ e a4 gis8[ fis] |
    
    e4( dis) s b' |
    gis8 gis gis gis b4 a8 gis |
    b a cis a gis4 gis8~ gis |
    
    %page2
    gis8[ ais] b~ b b4 ais |
    b2 s4 fis |
    fis fis fis r |
    
    r2 r4 fis |
    gis gis gis r |
    r2 r4 gis |
    
    fis8~ fis fis4 fis r |
    r2 r4 fis8~ fis |
    gis b ais gis ais cis b ais |
    
    %page3
    b4 b, b s4 |
    s1*2 |
    
    s1*3 |
    
    s1*2 |
  }
  \alternative {
    {
      s1
    }
    {
      s1
    }
  }
  %page4
}
altoWords = \lyricmode {
}
altoWordsII = \lyricmode {
%\markup\italic
  \set stanza = #"2. "
}
altoWordsIII = \lyricmode {
  \set stanza = #"3. "
}
altoWordsIV = \lyricmode {
  \set stanza = #"4. "
}
altoWordsV = \lyricmode {
  \set stanza = #"5. "
  \set ignoreMelismata = ##t
}
altoWordsVI = \lyricmode {
  \set stanza = #"6. "
  \set ignoreMelismata = ##t
}
tenorMusic = \relative c' {
  \repeat volta 2 {
    s8 b8 |
    e,4 e e e8 e |
    dis4 b e d8\rest cis |
    gis4 gis8~ gis a4 a |
    
    b2 d4\rest b'4 |
    e,8 e e e e4 e8 e |
    a,8 a a cis e4 e8~ e |
    
    %page2
    \slurDashed cis4 dis8( e) fis4 fis, |
    \slurSolid b b8[ cis] dis4 b |
    fis'2 e4. e8 |
    
    \tieSolid dis2~ dis8 b cis dis |
    e2 cis |
    ais4 cis8 cis fis4 e |
    
    dis8[ fis] e dis \slurDashed cis( e) dis[ cis] |
    b4 b8 cis dis4 dis |
    \tieDashed e cis8~ cis fis4 fis,8~ fis |
    
    %page 3
    b8 e dis cis b[ a] << {\set midiInstrument = #"flute" \voiceOne \stemUp b'4\fermata | <gis e>} \\ \context Voice = "tenors" {\set midiInstrument = #"flute" \voiceTwo \stemDown gis,8[ fis]^\fermata |
    e4}>> \oneVoice \stemNeutral <gis' e>4 q q8 q |
    q4 q q q |
    
    <a e>4 q q q8 q |
    q4 q q q |
    <gis e>8~ q q4 <a fis> q8 q |
    
    <b gis>4 q q b8~ b |
    cis8 e dis cis b a gis fis |
  }
  \alternative {
    {
      gis4 e e d4\rest\fermata |
    }
    {
      gis4 e e <gis b> |
    }
  }
  
  <a cis>2 |
  <a b,> |
  
  <gis cis,>2 d4\rest <gis b> |
  <a cis>2 <a b,> |
  <gis cis,> d4\rest <gis b> |
  <a cis>1 |
  <a b,>_\fermata |
  <gis e>2.
}

tenorWords = \lyricmode {
  \set ignoreMelismata = ##t
  \repeat unfold 37\skip1
  His _ nose should pant,
  And his lip _ should curl,
  His cheeks should flame,
  And his brow should furl, _
  And his bo -- som should _ heave,
  And his heart should glow,
  And his fist ev -- er rea -- dy for a knock -- _ down _ blow.
  
}

tenorWordsII = \lyricmode {
  \set ignoreMelismata = ##t
  \repeat unfold 37\skip1
  \set ignoreMelismata = ##t
  His _ foot should stamp,
  And his throat _ should growl,
  His hair should twirl,
  And his face should scowl, _
  And his eyes _ should _ flash,
  And his breast pro -- trude,
  And _ this his _ cus -- tom -- a -- ry at -- _ ti -- _ tude.
  
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
}
bassWords = \lyricmode {
}

pianoRH = \relative c' {
  \partial 4
  b'4 |
  <gis e'>4.( <a fis'>8) <b dis gis>4.( <a fis'>8) |
  <gis e'>4( <cis e cis'> <b e b'>) gis'8[( e]) |
  dis([ cis <a e'> cis]) <b gis>[( gis b e]) |
  << {gis4.( fis8 s4)} \\ {<dis a>2 <e gis,>4} >> \bar"||:" \break
}
pianoLH = \relative c' {
  \partial 4
  b4 |
  <e e,>2( <b b,>) |
  <cis cis,>4( <a a,> <gis gis,>) r |
  r <a cis e>( <b e>8) r r4 |
  <b b,>2_(~ <b e,>4) \bar"||:"
}
pianoRHend = \relative c' {
  \clef "treble"
  <b' b'>4 |
  q q q cis'8[ dis] |
  <e e,>4 <dis dis,> <cis cis,> <b b,> |
  <cis cis,> q q dis8[ e] |
  <fis fis,>4 <e e,> <dis dis,> <cis cis,> |
  
  <b b,>4 q q cis8[ dis] |
  <e e,>4 <dis dis,> <cis cis,> <b b,> |
  cis8[ e dis cis] b[ a gis fis] |
  e4 r <e gis e'> r |
  <e gis, e>\fermata r r2 \bar"|."
}
pianoLHend = \relative c' {
  <b gis e>4 |
  q q q q |
  q q q q |
  <cis a e> q q q |
  <dis b a e> q q q |
  <e b gis e> q <dis b a fis> q |
  <e b gis> q q q |
  <e cis a> q <dis b> q |
  <e e,> r <e, e,> r |
  q\fermata r r2 \bar"|."
}

\score {
<<
   \new ChoirStaff <<
    \new Staff = women {
      \global 
      \key e \major
      \time 4/4
      \new Voice = "pianoRH" {\pianoRH}
      \clef "treble_8"
      <<
        \new Voice = "sopranos" { \voiceOne << \global \sopMusic >> }
        \new Voice = "altos" { \voiceTwo << \global \altoMusic >> }
      >>
      \context Voice = "pianoRH" {\pianoRHend}
    }
    \new Lyrics = "altos"  \lyricsto "altos" \sopWords
    \new Lyrics = "altosII"  \lyricsto "altos" \sopWordsII
    \new Lyrics = "altosIII"  \lyricsto "sopranos" \sopWordsIII
    \new Lyrics = "altosIV"  \lyricsto "sopranos" \sopWordsIV
    \new Lyrics = "altosV"  \lyricsto "sopranos" \sopWordsV
   \new Staff = men {
      \clef bass
      \key e \major
      \time 4/4
      \new Voice = "pianoLH" {\set midiInstrument = #"acoustic grand" \pianoLH}
      \new Voice = "tenors" { << \global \tenorMusic >> }
      \context Voice = "pianoLH" {\pianoLHend}
    }
    \new Lyrics \with { alignBelowContext = #"men" } \lyricsto "tenors" \tenorWordsIII
    \new Lyrics \with { alignBelowContext = #"men" } \lyricsto "tenors" \tenorWordsII
    \new Lyrics \with { alignBelowContext = #"men" } \lyricsto "tenors" \tenorWords
  >>
%    \new PianoStaff << \new Staff { \new Voice { \pianoRH } } \new Staff { \clef "bass" \pianoLH } >>
  >>
  \layout {
    \context {
      \Lyrics
      \override LyricText #'font-size = #1.3
      \override VerticalAxisGroup #'staff-affinity = #0
      \override LyricText #'X-offset = #center-on-word
    }
    \context {
      \Score
      \override SpacingSpanner #'base-shortest-duration = #(ly:make-moment 1 8)
      \override SpacingSpanner #'common-shortest-duration = #(ly:make-moment 1 4)
    }
    \context {
      % Remove all empty staves
      \Staff \RemoveEmptyStaves \override VerticalAxisGroup #'remove-first = ##t
      
      \override VerticalAxisGroup #'staff-staff-spacing =
      #'((basic-distance . 0)
         (minimum-distance . 0)
         (padding . 1)
         (stretchability . 2))
    }
  }
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"A British Tar"}}
  poet = \markup\oldStyleNum"W. S. Gilbert (1836–1911)"
  composer = \markup\oldStyleNum"Arthur Sullivan (1842–1900)"
  tagline = ""
}}




