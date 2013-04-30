\version "2.14.2"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"The Spacious Firmament on High"}}
  poet = \markup\oldStyleNum"Joseph Addison (1672–1719)"
  composer = \markup\oldStyleNum"Franz Josef Haydn (1732–1809)"
  tagline = ""
}
\paper {
  %print-all-headers = ##t
  paper-height = 11\in
  paper-width = 8.5\in
  indent = 0\in
  %system-system-spacing = #'((basic-distance . 10) (padding . 0))
  system-system-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -3)
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
  \key aes \major
  \time 4/4
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
	\partial 4
  ees4 |
  aes2. aes4 |
  bes2. bes4 |
  c( aes) f( bes) |
  aes2( g4) ees8[ f] |
  g4( aes) bes( c) |
  des2. c4 |
  
  c( bes) bes( aes) |
  aes2( g4) ees |
  aes2. aes4 |
  bes2. bes4 |
  c( aes) f( bes) |
  aes2( g4) ees8[ f] |
  g4( aes) bes( c) |
  
  des2. c4 |
  c( bes) aes( g) |
  aes2. ees4 |
  ees2. ees4 |
  aes2. aes4 |
  bes2. bes4 |
  c2. bes4 |
  
  c( des ees) c |
  bes( c des) bes|
  c( des ees) c |
  bes2. ees,4 |
  ees2. ees4 |
  aes2. aes4 |
  bes2. bes4 |
  c2. bes4 |
  c( des ees) c |
  bes( c des) bes |
  aes2^\markup\italic"rall."  g |
  aes2. \bar"|."
}
sopWords = \lyricmode {
  \set stanza = #"1. "
	The spa -- cious firm -- a -- ment on __ high,
  And all __ \set associatedVoice = "altos" the blue \unset associatedVoice e -- _ the -- real sky, __
  And span -- gled heav’ns, a shin -- ing frame,
  Their great \set associatedVoice = "altos" O -- ri -- \unset associatedVoice gi -- _ nal pro -- claim.
  Th’un wear -- ied sun from day to day
  Does his __ Cre -- a -- tor’s pow’r dis -- play,
  \set associatedVoice = "altos"
  And pub -- lish -- es __ to ev -- ’ry land
  \unset associatedVoice
  The work __ of an __ al -- might -- y hand.
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  Soon as the eve -- ning shades pre -- vail,
  The moon \set associatedVoice = "altos" takes up \unset associatedVoice the "" won -- drous tale, __
  And night -- ly to the list -- ’ning earth
  Re -- peats \set associatedVoice = "altos" the sto -- \unset associatedVoice ry "" of __ her birth;
  Whilst all the stars that round her burn,
  And all __ the plan -- ets in __ their turn,
  \set associatedVoice = "altos" 
  Con -- firm the tid -- ings as they roll,
  \unset associatedVoice
  And spread the truth from pole to pole.
}

sopWordsIII = \lyricmode {
  \set stanza = #"3. "
  What though, in sol -- emn si -- lence, all
  Move round \set associatedVoice = "altos" the dark \unset associatedVoice ter -- "" rest -- rial ball;
  What though nor real "" voice, nor sound
  A -- midst \set associatedVoice = "altos" their ra -- \unset associatedVoice diant "" orbs be __ found;
  In rea -- son’s ear they all re -- joice,
  And ut -- ter forth a glo -- rious voice;
  \set associatedVoice = "altos" 
  For ev -- er sing -- ing as they shine:
  \unset associatedVoice
  “The hand __ that made __ us is div -- ine.”
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
}

altoMusic = \relative c' {
  \partial 4
  ees4 |
  c2. ees4 |
  ees2. ees4 |
  ees2 f |
  ees2. ees4 |
  ees2 ees |
  ees ees |
  
  f f |
  ees2. ees4 |
  c2. ees4 |
  ees2. ees4 |
  ees2 f |
  ees2. ees4 |
  ees2 ees |
  
  ees ees |
  f ees4( des) |
  c2. ees4 |
  ees2. ees4 |
  ees2. ees4 |
  ees2. ees4 |
  ees2. ees4 |
  
  ees2. ees4 |
  ees2. ees4 |
  ees2. ees4 |
  ees2. ees4 |
  ees2. ees4 |
  ees( des ees) c |
  
  ees2. ees4 |
  ees2. ees4 |
  ees2. aes4 |
  f( ees f) f |
  ees2 des |
  c2. \bar"|."
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
  \partial 4
  ees,4 |
  ees2. aes4 |
  g2. g4 |
  aes2 aes4( des) |
  c2( bes4) g8[ aes] |
  bes4( aes) g( aes) |
  bes2 aes |
  
  f f4( bes) |
  bes2. ees,4 |
  ees2. aes4 |
  g2. g4 |
  aes2 aes4( des) |
  c2( bes4) g8[ aes] |
  bes4( aes) g( aes) |
  
  bes2 aes |
  aes4( des) c( bes) |
  aes2. ees4 |
  ees2. g4 |
  aes2. aes4 |
  g2. g4 |
  aes2. g4 |
  
  aes4( bes c) aes |
  g( aes bes) g |
  aes( bes c) aes |
  g2. g4 |
  g2. bes4 |
  aes2. aes4 |
  
  g( aes bes) g |
  aes2. g4 |
  aes2. ees'4 |
  des( ees des) des |
  c2 bes |
  aes2. \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  \partial 4 
  ees,4 |
  aes,2. c4 |
  ees2. ees4 |
  aes( c,) des( bes) |
  ees2. ees4 |
  des( c) bes( aes) |
  g2 aes |
  
  des d |
  ees2. ees4 |
  aes,2. c4 |
  ees2. ees4 |
  aes( c,) des( bes) |
  ees2. ees4 |
  des( c) bes( aes) |
  
  g2 aes |
  des ees |
  aes,2. ees'4 |
  ees2. ees4 |
  c2. c4 |
  ees2. ees4 |
  aes2. ees4 |
  
  ees2. ees4 |
  ees2. ees4 |
  ees2. ees4 |
  ees2. ees4 |
  ees( f ees) des |
  c( bes c) aes |
  
  ees'( f g) ees |
  aes( ees c) ees |
  aes( bes c) aes |
  des( c bes) des, |
  ees2 ees |
  aes,2. \bar"|."
}
bassWords = \lyricmode {
}

pianoRH = \relative c' {
}
pianoLH = \relative c' {
}

\score {
<<
   \new ChoirStaff <<
    \new Staff = women <<
      \new Voice = "sopranos" { \voiceOne << \global \sopMusic >> }
      \new Voice = "altos" { \voiceTwo << \global \altoMusic >> }
    >>
    \new Lyrics = "altos"  \lyricsto "sopranos" \sopWords
    \new Lyrics = "altosII"  \lyricsto "sopranos" \sopWordsII
    \new Lyrics = "altosIII"  \lyricsto "sopranos" \sopWordsIII
    \new Lyrics = "altosIV"  \lyricsto "sopranos" \sopWordsIV
    \new Lyrics = "altosV"  \lyricsto "sopranos" \sopWordsV
   \new Staff = men <<
      \clef bass
      \new Voice = "tenors" { \voiceOne << \global \tenorMusic >> }
      \new Voice = "basses" { \voiceTwo << \global \bassMusic >> }
    >>
    \new Lyrics \with { alignAboveContext = #"tenors" } \lyricsto "tenors" \tenorWordsIII
    \new Lyrics \with { alignAboveContext = #"tenors" } \lyricsto "tenors" \tenorWordsII
    \new Lyrics \with { alignAboveContext = #"tenors" } \lyricsto "tenors" \tenorWords
  >>
%    \new PianoStaff << \new Staff { \new Voice { \pianoRH } } \new Staff { \clef "bass" \pianoLH } >>
  >>
  \midi {
    \tempo 4 = 130
    \set Staff.midiInstrument = "flute"
  
    \context {
      \Voice
      \remove "Dynamic_performer"
    }
  }
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
         (padding . -1)
         (stretchability . 2))
    }
  }
}


